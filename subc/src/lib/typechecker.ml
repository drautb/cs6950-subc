open Core
open Printf

open Ast

exception TypeError of string;;

let main_fn_arglist = ArgList [(Int, ""); (Array (Pointer Char), "")]

let make_fn_table () =
  Hashtbl.create ~hashable:String.hashable ()
;;

let lookup_fn fn_table name =
  Hashtbl.find fn_table name
;;

let string_of_fn_table fn_table =
  Hashtbl.fold
    fn_table
    ~init:"\n"
    ~f:(fun ~key:k ~data:v acc ->
        match v with
        | (ret_type, args, defined, valid_return) ->
          acc ^ (sprintf "Name: %s | RET: %s    ARGS: %s    DEFINED: %s    VALID RET: %s\n"
                   k
                   (string_of_subc_type ret_type)
                   (string_of_arg_list args)
                   (string_of_bool defined)
                   (string_of_bool valid_return)))
;;

(* Saves an entry to the function table. defined and valid_return are just flags
   that indicate whether or not a definition for this function has already been
   found, and whether or not the function has a valid return statement if it is
   a non-void function, respectively. *)
let save_fn fn_table name ret_type args (defined : bool) (valid_return : bool) =
  Hashtbl.set fn_table ~key:name ~data:(ret_type, args, defined, valid_return);
;;

let make_scope () =
  Hashtbl.create ~hashable:String.hashable ()
;;

(* Given a list of scope hashes and an identifier, lookup the id in the scopes
   and return it's type. *)
let rec lookup_id
    (scopes : (String.t, subc_type) Hashtbl.t list)
    (id : string) : subc_type option =
  match scopes with
  | [] -> None
  | scope :: rest -> (match Hashtbl.find scope id with
      | None -> (lookup_id rest id)
      | Some id_type -> Some id_type)
;;

let add_to_scope scopes (id : string) (id_type : subc_type) =
  match scopes with
  | [] -> raise (Failure "empty scope list")
  | scope :: _ -> (match Hashtbl.find scope id with
      | None -> Hashtbl.set scope ~key:id ~data:id_type
      | Some _ -> raise (TypeError (sprintf "Redefinition of '%s'" id)))
;;

(* Given actual lists of argument types, return true if their length and types match. *)
let argument_types_match
    (args1 : (subc_type * string) list)
    (args2 : (subc_type * string) list) =
  match List.length args1 = List.length args2 with
  | false -> false
  | true ->
    List.fold ~init:true ~f:(&&)
            (List.map2_exn args1 args2
               ~f:(fun arg1 arg2 -> match arg1 with
                   | (type1, _) -> (match arg2 with
                       | (type2, _) -> (type1 = type2))))
;;

(* Given two arglists, returns true if they have the same number and type of
   arguments. Names need not match *)
let compare_arglists (arg_list1 : arg_list) (arg_list2 : arg_list) =
  match arg_list1 with
    | ArgVoid -> (match arg_list2 with
        | ArgVoid -> true
        | ArgList _ -> false)
    | ArgList args1 -> (match arg_list2 with
        | ArgVoid -> false
        | ArgList args2 -> argument_types_match args1 args2)
;;

(* Given an argument list, ensure that no two parameters share a name. *)
let ensure_unique_parameter_names (fn_name : string) (arg_list : arg_list) =
  match arg_list with
  | ArgVoid -> ()
  | ArgList args -> let unique_parameter_names =
                      List.dedup (List.map args ~f:(fun arg -> match arg with
                          | (_, name) -> name)) in
    match List.length unique_parameter_names = List.length args with
    | true -> ()
    | false -> raise (TypeError (sprintf "Function '%s' parameter names are not unique" fn_name))
;;

(* Ensures that correctly defined main function is present. *)
let validate_main_fn fn_table =
  match lookup_fn fn_table "main" with
  | None -> raise (TypeError "No 'main' function declared or defined")
  | Some (ret_type, args, defined, _) ->
    match ret_type with
    | Int -> (match compare_arglists args main_fn_arglist with
        | true -> (match defined with
            | true -> ()
            | false -> raise (TypeError "No definition found for 'main' function"))
        | false -> raise (TypeError "'main' function argument list does not match required argument list"))
    | _ -> raise (TypeError "'main' function does not have 'int' return type")
;;

(* Given two types, return whether or not they're compatible. *)
let rec compatible_types (t1 : subc_type) (t2 : subc_type) : bool =
  match t1 with
  | Void -> (match t2 with
      | Void -> true
      | _ -> false)
  | Int -> (match t2 with
      | Int | Char -> true
      | _ -> false)
  | Char -> (match t2 with
      | Int | Char -> true
      | _ -> false)
  | Bool -> (match t2 with
      | Bool -> true
      | _ -> false)
  | Pointer nested_type1 -> (match t2 with
      | Pointer nested_type2 -> compatible_types nested_type1 nested_type2
      | _ -> false)
  | Array nested_type1 -> (match t2 with
      | Array nested_type2 -> compatible_types nested_type1 nested_type2
      | _ -> false)
;;

let typecheck_function_declaration fn_table ret_type name args =
  (* A function can only be declared once. *)
  let _ = match lookup_fn fn_table name with
  | None -> ()
  | Some _ -> raise (TypeError (sprintf "Function '%s' has already been declared" name)) in

  (* Prameter names may not be repeated within a function. *)
  ensure_unique_parameter_names name args;

  (* Store the function in the table. *)
  save_fn fn_table name ret_type args false false
;;

let typecheck_variable_declaration scopes var_type id =
  add_to_scope scopes id var_type
;;

let typecheck_array_declaration scopes var_type id size =
  let _ = match size > 0 with
    | true -> ()
    | false -> raise (TypeError (sprintf "Array '%s' declared with non-natural size '%d'" id size)) in
  add_to_scope scopes id var_type
;;

let typecheck_declaration fn_table scopes declaration =
  match declaration with
  | VariableDeclaration (var_type, id) ->
    typecheck_variable_declaration scopes var_type id
  | ArrayDeclaration (var_type, id, size) ->
    typecheck_array_declaration scopes var_type id size
  | FunctionDeclaration (ret_type, name, args) ->
    typecheck_function_declaration fn_table ret_type name args
;;

let rec typecheck_expression fn_table scopes (expr : expression) : subc_type =
  match expr with
  | Assignment (lhs, rhs) -> begin
      let lhs_type = typecheck_expression fn_table scopes lhs in
      let rhs_type = typecheck_expression fn_table scopes rhs in
      let _ = match lhs_type with
        | Array _ -> raise (TypeError "An array is not a valid left-hand side in an assignment")
        | _ -> () in
      let _ = match lhs with
        | Id _ | ArrayRef (_, _) | Dereference _ -> ()
        | _ -> raise (TypeError "Left-hand side of assignment is not an assignable expression.") in
      match compatible_types lhs_type rhs_type with
      | true -> Void
      | false -> raise (TypeError "Right-hand side of assignment is not compatible with left-hand side")
    end
  | LogicalOr (lhs, rhs)
  | LogicalAnd (lhs, rhs) -> begin
      let lhs_type = typecheck_expression fn_table scopes lhs in
      let rhs_type = typecheck_expression fn_table scopes rhs in
      match lhs_type, rhs_type with
      | Bool, Bool -> Bool
      | _ -> raise (TypeError "Operands to logical and operators must be of type bool")
    end
  | LogicalNot expr -> (match typecheck_expression fn_table scopes expr with
      | Bool -> Bool
      | _ -> raise (TypeError "Operand to logical not must be of type bool"))
  | Equal (lhs, rhs)
  | NotEqual (lhs, rhs)
  | LessThan (lhs, rhs)
  | LessThanEqual (lhs, rhs)
  | GreaterThan (lhs, rhs)
  | GreaterThanEqual (lhs, rhs) -> ensure_int_compatibility fn_table scopes lhs rhs Bool
  | Add (lhs, rhs)
  | Subtract (lhs, rhs)
  | Multiply (lhs, rhs)
  | Divide (lhs, rhs) -> ensure_int_compatibility fn_table scopes lhs rhs Int
  | Cast (new_type, expr) ->
    let _ = typecheck_expression fn_table scopes expr in new_type
  | ArrayRef (array_expr, idx_expr) -> begin
      let idx_type = typecheck_expression fn_table scopes idx_expr in
      let _ = match compatible_types idx_type Int with
        | true -> ()
        | false -> raise (TypeError "array index expression does not have a type compatible with int") in
      match typecheck_expression fn_table scopes array_expr with
      | Array entry_type -> entry_type
      | _ -> raise (TypeError "identifier is not an array")
    end
  | FunctionCall (name_expr, actual_args) -> begin
      match name_expr with
      | Id fn_name -> (match lookup_fn fn_table fn_name with
          | Some (ret_type, args, _, _) ->
            let _ = ensure_argument_compatibility fn_table scopes args actual_args in ret_type
          | None -> raise (TypeError (sprintf "No declaration found for function '%s'" fn_name)))
      | _ -> raise (TypeError "Function name expression in function call is not an identifier")
    end
  | Id name ->
    (match lookup_id scopes name with
     | None -> raise (TypeError (sprintf "Variable '%s' can't be used before being declared" name))
     | Some id_type -> id_type)
  | IntConst _ -> Int
  | CharConst _ -> Char
  | AddressOf expr -> Pointer (typecheck_expression fn_table scopes expr)
  | Dereference expr ->
      (match typecheck_expression fn_table scopes expr with
        | Pointer pointer_type -> pointer_type
        | _ -> raise (TypeError (sprintf "Dereference operator may only be applied to a pointer")))
  | Negate expr ->
    let expr_type = typecheck_expression fn_table scopes expr in
    match compatible_types expr_type Int with
    | true -> Int
    | false -> raise (TypeError "Cannot negate an expression that has type incompatible with int")

and ensure_int_compatibility fn_table scopes
    (lhs : expression)
    (rhs : expression)
    (overall : subc_type) : subc_type =
  let lhs_type = typecheck_expression fn_table scopes lhs in
  let rhs_type = typecheck_expression fn_table scopes rhs in
  let _ = match compatible_types lhs_type Int, compatible_types rhs_type Int with
    | true, true -> ()
    | _, _ -> raise (TypeError "Operands to comparison and mathematic operators must be compatible with type int") in
  overall

and ensure_argument_compatibility fn_table scopes
    (args : arg_list)
    (actual_args : expression list) : unit =
  match args with
  | ArgVoid -> (match List.length actual_args with
      | 0 -> ()
      | _ -> raise (TypeError "Function takes no arguments"))
  | ArgList arg_tuples -> begin
      let _ = match List.length arg_tuples = List.length actual_args with
        | true -> ()
        | false -> raise (TypeError "Function call has a different number of arguments than the function declaration") in
      let _ = List.map2_exn arg_tuples actual_args
          ~f:(fun (expected_type, _) arg_expr ->
              match compatible_types expected_type (typecheck_expression fn_table scopes arg_expr) with
              | true -> ()
              | false -> raise (TypeError "One or more actual arguments to function are incompatible with argument types")) in ()
    end
;;

let typecheck_return fn_table scopes (ret_type : subc_type) name ret_expr =
  match ret_expr with
  | None -> (match ret_type with
      | Void -> ()
      | _ -> raise (TypeError "non-void function must return a value"))
  | Some expr -> (match ret_type with
      | Void -> raise (TypeError "void function may not return a value")
      | _ -> let ret_expr_type = typecheck_expression fn_table scopes expr in
        (match compatible_types ret_type ret_expr_type with
         | true -> (match lookup_fn fn_table name with
             | Some (ret_type, args, defined, _) -> save_fn fn_table name ret_type args defined true
             | None -> save_fn fn_table name Void ArgVoid false true)
         | false -> raise (TypeError "return expression is not compatible with declared return type")))
;;

(* Assuming a new scope has already been created, this function typechecks the list
   of declarations and statements that comprise the block. *)
let rec typecheck_block fn_table scopes ret_type name args block_decls block_stmts =
  let _ = List.map block_decls
      ~f:(fun decl -> typecheck_declaration fn_table scopes decl) in
  let _ = List.map block_stmts
      ~f:(fun stmt -> typecheck_statement fn_table scopes ret_type name args stmt) in ()

and typecheck_statement fn_table scopes ret_type name args stmt : unit =
  match stmt with
  | Block (decls, stmts) ->
    (* Start a new scope in a new block *)
    let scopes = make_scope () :: scopes in
    typecheck_block fn_table scopes ret_type name args decls stmts
  | Conditional (expr, then_stmt, else_stmt) ->
    (match typecheck_expression fn_table scopes expr with
     | Bool -> begin
         let _ = typecheck_statement fn_table scopes ret_type name args then_stmt in
         match else_stmt with
         | None -> ()
         | Some stmt -> typecheck_statement fn_table scopes ret_type name args stmt
       end
     | _ -> raise (TypeError "Conditional expression must be of type bool"))
  | Expression expr ->
    let _ = typecheck_expression fn_table scopes expr in ()
  | Loop (expr, stmt) ->
    (match typecheck_expression fn_table scopes expr with
     | Bool -> typecheck_statement fn_table scopes ret_type name args stmt
     | _ -> raise (TypeError "Loop expression must be of type bool"))
  | Return expr -> typecheck_return fn_table scopes ret_type name expr
  | StmtVoid -> ()
;;

let add_args_to_scopes scopes (arg_list : arg_list) =
  match arg_list with
  | ArgVoid -> ()
  | ArgList args -> let _ = List.map args
                        ~f:(fun arg -> match arg with
                            | (var_type, id) -> add_to_scope scopes id var_type) in ()
;;

let typecheck_function_body fn_table scopes ret_type name args stmt =
  match stmt with
  | Block (decls, stmts) ->
    (* Start a new scope for the function body, including the arguments in the new scope. *)
    let scopes = make_scope () :: scopes in
    add_args_to_scopes scopes args;
    typecheck_block fn_table scopes ret_type name args decls stmts
  | _ -> raise (TypeError "A function body must begin with a block statement")
;;

let typecheck_function_definition fn_table scopes ret_type name args stmt =
  (* A function can only be defined once, and if a declaration exists, the return
     type and formal argument list must match in type and number. *)
  let _ = match lookup_fn fn_table name with
    | None -> ()
    | Some (ret_type_decl, args_decl, defined, _) ->
      (match defined with
       | true ->
         raise (TypeError (sprintf "Function '%s' has already been defined" name))
       | false -> (match ret_type_decl = ret_type with
           | false ->
             raise (TypeError (sprintf "Function '%s' declared return type does not match the definition" name))
           | true -> (match compare_arglists args_decl args with
               | false ->
                 raise (TypeError (sprintf "Function '%s' declared argument list does not match the definition" name))
               | true -> ()))) in

  (* Parameter names must be unique *)
  ensure_unique_parameter_names name args;

  (* Ensure that the body of the function typechecks. *)
  typecheck_function_body fn_table scopes ret_type name args stmt;

  (* Make sure that the function had a correct return statement if it was non-void *)
  let _ = match ret_type with
  | Void -> ()
  | _ -> (match lookup_fn fn_table name with
      | Some (_, _, _, true) -> ()
      | _ -> raise (TypeError (sprintf "Function '%s' must return a value" name))) in

  (* Store the function in the function table. *)
  save_fn fn_table name ret_type args true true
;;

let typecheck_subc_unit fn_table scopes subc_unit =
  match subc_unit with
  | Declaration decl ->
    typecheck_declaration fn_table scopes decl
  | FunctionDefinition (ret_type, name, args, stmt) ->
    typecheck_function_definition fn_table scopes ret_type name args stmt
;;

let typecheck_ast fn_table scopes ast =
  let _ = match ast with
    | Ast unit_list ->
      List.map unit_list ~f:(fun u -> typecheck_subc_unit fn_table scopes u) in ()
;;

let typecheck ast =
  let fn_table = make_fn_table () in
  let scopes = [make_scope ()] in
  typecheck_ast fn_table scopes ast;

  validate_main_fn fn_table
;;
