open Core
open Printf

open Ast

exception TypeError of string;;

let main_fn_arglist = ArgList [(Int, "", false); ((Pointer Char), "", true)]

let make_fn_table () =
  Hashtbl.create ~hashable:String.hashable ()
;;

let lookup_fn fn_table name =
  Hashtbl.find fn_table name
;;

(* Saves an entry to the function table. defined and valid_return are just flags
   that indicate whether or not a definition for this function has already been
   found, and whether or not the function has a valid return statement if it is
   a non-void function, respectively. *)
let save_fn fn_table name ret_type args (defined : bool) (valid_return : bool) =
  Hashtbl.set fn_table ~key:name ~data:(ret_type, args, defined, valid_return)
;;

let make_scope () =
  Hashtbl.create ~hashable:String.hashable ()
;;

(* Given a list of scope hashes and an identifier, lookup the id in the scopes
   and return it's type. *)
let rec lookup_id scopes (id : string) : subc_type option =
  match scopes with
  | [] -> None
  | scope :: rest -> (match Hashtbl.find scope id with
      | None -> (lookup_id rest id)
      | Some id_type -> id_type)
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
    (args1 : (subc_type * string * bool) list)
    (args2 : (subc_type * string * bool) list) =
  match List.length args1 = List.length args2 with
  | false -> false
  | true -> List.fold ~init:true ~f:(&&)
            (List.map2_exn args1 args2
               ~f:(fun arg1 arg2 -> match arg1 with
                   | (type1, _, is_array1) -> (match arg2 with
                       | (type2, _, is_array2) ->
                     (type1 = type2 && is_array1 = is_array2))))
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
                          | (_, name, _) -> name)) in
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
  | Variable (var_type, id) ->
    typecheck_variable_declaration scopes var_type id
  | Array (var_type, id, size) ->
    typecheck_array_declaration scopes var_type id size
  | FunctionDeclaration (ret_type, name, args) ->
    typecheck_function_declaration fn_table ret_type name args
;;

let typecheck_expression fn_table _ ret_type name args expr =
  match expr with
   | Equal (_, _) -> raise (TypeError "return expression does not conform to declared return type")
   | _ -> save_fn fn_table name ret_type args false true
;;

let typecheck_return fn_table scopes (ret_type : subc_type) name args ret_expr =
  match ret_expr with
  | None -> (match ret_type with
      | Void -> ()
      | _ -> raise (TypeError "non-void function must return a value"))
  | Some expr -> (match ret_type with
      | Void -> raise (TypeError "void function may not return a value")
      | _ -> typecheck_expression fn_table scopes ret_type name args expr)
;;

(* Assuming a new scope has already been created, this function typechecks the list
   of declarations and statements that comprise the block. *)
let rec typecheck_block fn_table scopes ret_type name args stmt block_decls block_stmts =
  let _ = List.map block_decls
      ~f:(fun decl -> typecheck_declaration fn_table scopes decl) in
  let _ = List.map block_stmts
      ~f:(fun stmt -> typecheck_statement fn_table scopes ret_type name args stmt) in ()

and typecheck_statement fn_table scopes ret_type name args stmt =
  match stmt with
  | Block (decls, stmts) ->
    (* Start a new scope in a new block *)
    let scopes = make_scope () :: scopes in
    typecheck_block fn_table scopes ret_type name args stmt decls stmts
  | Conditional (_, _, _) -> ()
  | Expression _ -> ()
  | Loop (_, _) -> ()
  | Return expr -> typecheck_return fn_table scopes ret_type name args expr
  | StmtVoid -> ()
;;

let add_args_to_scopes scopes (arg_list : arg_list) =
  match arg_list with
  | ArgVoid -> ()
  | ArgList args -> let _ = List.map args
                        ~f:(fun arg -> match arg with
                            | (var_type, id, _) -> add_to_scope scopes id var_type) in ()
;;

let typecheck_function_body fn_table scopes ret_type name args stmt =
  match stmt with
  | Block (decls, stmts) ->
    (* Start a new scope for the function body, including the arguments in the new scope. *)
    let scopes = make_scope () :: scopes in
    add_args_to_scopes scopes args;
    typecheck_block fn_table scopes ret_type name args stmt decls stmts
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