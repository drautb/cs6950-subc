open Core
open Printf

open Ast

exception TypeError of string;;

let main_fn_arglist = ArgList [(Int, "", false); ((Pointer Char), "", true)]

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
  match Hashtbl.find fn_table "main" with
  | None -> raise (TypeError "No 'main' function declared or defined")
  | Some (ret_type, args, defined) ->
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
  let _ = match Hashtbl.find fn_table name with
  | None -> ()
  | Some _ -> raise (TypeError (sprintf "Function '%s' has already been declared" name)) in

  ensure_unique_parameter_names name args;

  Hashtbl.set fn_table ~key:name ~data:(ret_type, args, false)
;;

let typecheck_return fn_table ret_type ret_expr =
  match ret_expr with
  | None -> ()
  | Some _ -> ()
;;

let typecheck_statement fn_table ret_type stmt =
  match stmt with
  | Block (_, _) -> ()
  | Conditional (_, _, _) -> ()
  | Expression _ -> ()
  | Loop (_, _) -> ()
  | Return expr -> typecheck_return fn_table ret_type expr
  | StmtVoid -> ()
;;

let typecheck_function_definition fn_table ret_type name args stmt =
  (* A function can only be defined once, and if a declaration exists, the return
     type and formal argument list must match in type and number. *)
  let _ = match Hashtbl.find fn_table name with
    | None -> ()
    | Some (ret_type_decl, args_decl, defined) ->
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
  typecheck_statement fn_table ret_type stmt;

  (* Store the function in the function table. *)
  Hashtbl.set fn_table ~key:name ~data:(ret_type, args, true)
;;

let typecheck_declaration fn_table declaration =
  match declaration with
  | Variable (_, _) -> ()
  | Array (_, _, _) -> ()
  | FunctionDeclaration (ret_type, name, args) ->
    typecheck_function_declaration fn_table ret_type name args
;;

let typecheck_subc_unit fn_table subc_unit =
  match subc_unit with
  | Declaration decl -> typecheck_declaration fn_table decl
  | FunctionDefinition (ret_type, name, args, stmt) ->
    typecheck_function_definition fn_table ret_type name args stmt
;;

let typecheck_ast fn_table ast =
  let _ = match ast with
    | Ast unit_list -> List.map unit_list ~f:(fun u -> typecheck_subc_unit fn_table u) in ()
;;

let typecheck ast =
  let fn_table = Hashtbl.create ~hashable:String.hashable () in
  typecheck_ast fn_table ast;

  validate_main_fn fn_table
;;
