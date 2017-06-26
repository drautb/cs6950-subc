open Core
open Llvm

open Ast

let rec generate_llvm_type llctx subc_type =
  match subc_type with
    | Void -> void_type llctx
    | Int -> i32_type llctx
    | Char -> i8_type llctx
    | Bool -> i1_type llctx
    | Pointer t | Array t -> pointer_type (generate_llvm_type llctx t)
;;

let generate_llvm_arg_list llctx arg_list =
  match arg_list with
  | ArgVoid -> [| |]
  | ArgList args ->
    Array.of_list (List.map args ~f:(fun (arg_type, _) ->
        generate_llvm_type llctx arg_type))
;;

let rec generate_expression llctx llm llbuilder expr : llvalue =
  match expr with
  | Id _ ->
    let v = build_alloca (i32_type llctx) "1" llbuilder in
    let _ = set_alignment 4 v in v
  | IntConst n -> const_int (i32_type llctx) n
  | CharConst c -> const_int (i8_type llctx) (int_of_char c)
  | AddressOf expr -> generate_expression llctx llm llbuilder expr
  | _ -> const_int (i32_type llctx) 0
;;

let rec generate_statement llctx llm llbuilder stmt : unit =
  let _ = match stmt with
    | Block (_, stmts) ->
      let _ = List.map stmts ~f:(fun stmt -> generate_statement llctx llm llbuilder stmt) in ()
    | Return ret_expr -> (match ret_expr with
        | None -> let _ = build_ret_void llbuilder in ()
        | Some expr ->
          let ret_value = generate_expression llctx llm llbuilder expr in
          let _ = build_ret ret_value llbuilder in ())
    | _ -> raise (Failure "NOT DONE YET") in ()
;;

let generate_function_definition llctx llm ret_type name arg_list stmt : unit =
  let llvm_ret_type = generate_llvm_type llctx ret_type in
  let llvm_arg_list = generate_llvm_arg_list llctx arg_list in
  let fn_type = function_type llvm_ret_type llvm_arg_list in
  let fn = define_function name fn_type llm in
  let llbuilder = builder_at_end llctx (entry_block fn) in
  let _ = generate_statement llctx llm llbuilder stmt in ()
;;

let generate_subc_unit llctx llm (u : subc_unit) =
  match u with
  | Declaration _ -> ()
  | FunctionDefinition (ret_type, name, arg_list, stmt) ->
    generate_function_definition llctx llm ret_type name arg_list stmt
;;

let generate_module (module_name : string) (ast : ast) : llmodule =
  (* Initialize LLVM bits *)
  let llctx = global_context () in
  let llm = create_module llctx module_name in

  (* Build the module *)
  let _ = match ast with
  | Ast unit_list ->
    List.map unit_list ~f:(fun u -> generate_subc_unit llctx llm u) in

  (* Make sure it's valid *)
  Llvm_analysis.assert_valid_module llm;

  (* Return *)
  llm
;;
