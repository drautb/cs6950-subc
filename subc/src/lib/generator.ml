open Core
open Llvm
open Printf

open Ast

let generate_module (module_name : string) (ast : ast) : llmodule =
  let llctx = global_context () in
  let llm = create_module llctx module_name in

  let i32_t = i32_type llctx in
  let fn_type = function_type i32_t [| |] in
  let f = define_function "main" fn_type llm in

  let llbuilder = builder_at_end llctx (entry_block f) in
  let _ = build_ret (const_int i32_t 0) llbuilder in llm
;;
