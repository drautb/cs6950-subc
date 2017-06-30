open Core
open Llvm

open Ast

let todo () =
  raise (Failure "todo")
;;

exception IdNotFound of string;;

let make_scope () =
  Hashtbl.create ~hashable:String.hashable ()
;;

let rec lookup_id
    (scopes : (String.t, (llvalue * subc_type)) Hashtbl.t list)
    (id : string) : llvalue * subc_type =
  match scopes with
  | [] -> raise (IdNotFound id)
  | scope :: rest -> (match Hashtbl.find scope id with
      | None -> (lookup_id rest id)
      | Some value_and_type -> value_and_type)
;;

(* Given a list of scope hashes and an identifier, lookup the id in the scopes
   and return its llvalue. *)
let lookup_value
    (scopes : (String.t, (llvalue * subc_type)) Hashtbl.t list)
    (id : string) : llvalue =
  match lookup_id scopes id with
  | (llvalue, _) -> llvalue
;;

(* Given a list of scope hashes and an identifier, lookup the id in the scopes
   and return its subc_type *)
let lookup_type
    (scopes : (String.t, (llvalue * subc_type)) Hashtbl.t list)
    (id : string) : subc_type =
  match lookup_id scopes id with
  | (_, subc_type) -> subc_type
;;

let add_to_scope scopes (id : string) (llv : llvalue) (t : subc_type) : unit =
  match scopes with
  | [] -> raise (Failure "empty scope list")
  | scope :: _ -> (match Hashtbl.find scope id with
      | None -> Hashtbl.set scope ~key:id ~data:(llv, t)
      | Some _ -> todo ())
;;

let rec generate_llvm_type llctx subc_type =
  match subc_type with
    | Void -> void_type llctx
    | Int -> i32_type llctx
    | Char -> i8_type llctx
    | Bool -> i1_type llctx
    | Pointer t | Array t -> pointer_type (generate_llvm_type llctx t)
;;

let set_alignment llvalue subc_type : unit =
  match subc_type with
  | Void -> ()
  | Int -> set_alignment 4 llvalue
  | Char -> set_alignment 1 llvalue
  | Bool -> ()
  | Pointer _
  | Array _  -> set_alignment 8 llvalue

let generate_llvm_arg_list llctx arg_list =
  match arg_list with
  | ArgVoid -> [| |]
  | ArgList args ->
    Array.of_list (List.map args ~f:(fun (arg_type, _) ->
        generate_llvm_type llctx arg_type))
;;

let rec generate_expression llctx llm llbuilder scopes expr : llvalue =
  match expr with
  | Assignment (_, _)
  | LogicalOr (_, _)
  | LogicalAnd (_, _)
  | LogicalNot _
  | Equal (_, _)
  | NotEqual (_, _)
  | LessThan (_, _)
  | LessThanEqual (_, _)
  | GreaterThan (_, _)
  | GreaterThanEqual (_, _)
  | Add (_, _)
  | Subtract (_, _)
  | Multiply (_, _)
  | Divide (_, _)
  | Cast (_, _)
  | ArrayRef (_, _)
  | FunctionCall (_, _) -> todo ()
  | Id id_name ->
    let v = lookup_value scopes id_name in
    let _ = set_alignment v (lookup_type scopes id_name) in v
  | IntConst n -> const_int (i32_type llctx) n
  | CharConst c -> const_int (i8_type llctx) (int_of_char c)
  | AddressOf expr -> generate_expression llctx llm llbuilder scopes expr
  | Dereference _
  | Negate _ -> todo ()
;;

let generate_declaration llctx llm llbuilder scopes decl : unit =
  match decl with
  | VariableDeclaration (id_type, id_name) -> begin
      let id_llv = build_alloca (generate_llvm_type llctx id_type) "" llbuilder in
      add_to_scope scopes id_name id_llv id_type;
      set_alignment id_llv id_type;
    end
  | _ -> ()
;;


let rec generate_statement llctx llm llbuilder scopes stmt : unit =
  let _ = match stmt with
    | Block (decls, stmts) ->
      let _ = List.map decls ~f:(fun decl -> generate_declaration llctx llm llbuilder scopes decl) in
      let _ = List.map stmts ~f:(fun stmt -> generate_statement llctx llm llbuilder scopes stmt) in ()
    | Conditional _
    | Expression _
    | Loop _ -> todo ()
    | Return ret_expr -> (match ret_expr with
        | None -> let _ = build_ret_void llbuilder in ()
        | Some expr ->
          let ret_value = generate_expression llctx llm llbuilder scopes expr in
          let _ = build_ret ret_value llbuilder in ())
    | StmtVoid -> () in ()
;;

let allocate_function_memory llctx llm llbuilder scopes
    (fn : llvalue)
    (ret_type : subc_type)
    (arg_list : arg_list) : unit =
  (* Allocation for return value *)
  let ref_ret_value_lbl = ref (const_null (void_type llctx)) in
  let _ = match ret_type with
  | Void -> ()
  | _ -> begin
      let llvm_ret_type = generate_llvm_type llctx ret_type in
      let ret_value_lbl = build_alloca llvm_ret_type "" llbuilder in
      ref_ret_value_lbl := ret_value_lbl;
      set_alignment !ref_ret_value_lbl ret_type
    end in

  (* Allocations for function arguments *)
  let _ = match arg_list with
    | ArgVoid -> ()
    | ArgList args ->
      List.iter args ~f:(fun (arg_type, name) ->
          let lbl = build_alloca (generate_llvm_type llctx arg_type) "" llbuilder in
          set_alignment lbl arg_type;
          add_to_scope scopes name lbl arg_type) in

  (* Stores *)
  let _ = match ret_type with
    | Void
    | Pointer _ -> ()
    | _ -> let store_lbl = build_store (const_int (generate_llvm_type llctx ret_type) 0)
               !ref_ret_value_lbl llbuilder in
      set_alignment store_lbl ret_type in


  let _ = match arg_list with
    | ArgVoid -> ()
    | ArgList args ->
      List.iteri args ~f:(fun i (arg_type, name) ->
          let store_lbl = build_store
              (param fn i)
              (lookup_value scopes name)
              llbuilder in
          set_alignment store_lbl arg_type;)
  in ()
;;

let generate_function_definition llctx llm scopes ret_type name arg_list stmt : unit =
  (* Build function definition *)
  let llvm_ret_type = generate_llvm_type llctx ret_type in
  let llvm_arg_list = generate_llvm_arg_list llctx arg_list in
  let fn_type = function_type llvm_ret_type llvm_arg_list in
  let fn = define_function name fn_type llm in

  (* Create builder for function blocks *)
  let llbuilder = builder_at_end llctx (entry_block fn) in

  (* Allocate memory for return value and each argument *)
  let _ = allocate_function_memory llctx llm llbuilder scopes fn ret_type arg_list in

  (* Generate blocks for function body *)
  let _ = generate_statement llctx llm llbuilder scopes stmt in ()
;;

let generate_subc_unit llctx llm scopes (u : subc_unit) =
  match u with
  | Declaration _ -> ()
  | FunctionDefinition (ret_type, name, arg_list, stmt) ->
    generate_function_definition llctx llm scopes ret_type name arg_list stmt
;;

let generate_module (module_name : string) (ast : ast) : llmodule =
  (* Initialize LLVM bits *)
  let llctx = global_context () in
  let llm = create_module llctx module_name in

  (* Build the module *)
  let scopes = [make_scope ()] in
  let _ = match ast with
  | Ast unit_list ->
    List.map unit_list ~f:(fun u -> generate_subc_unit llctx llm scopes u) in

  (* Make sure it's valid *)
  Llvm_analysis.assert_valid_module llm;

  (* Return *)
  llm
;;
