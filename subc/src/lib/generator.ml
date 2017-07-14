open Core
open Llvm

open Ast

(* Manufactured variable name used to hold the return value for a function *)
let retval = "%retval";;

let todo str =
  raise (Failure ("todo: " ^ str))
;;

exception IdNotFound of string;;

type logical_cmp =
  | Or
  | And
;;

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
      | Some _ ->
        raise (Failure (sprintf "Attempting to redefine %s, typechecker should have prevented this." id)))
;;

let rec generate_llvm_type ?array_size:(array_size = -1) llctx subc_type =
  match subc_type with
    | Void -> void_type llctx
    | Int -> i32_type llctx
    | Char -> i8_type llctx
    | Bool -> i1_type llctx
    | Pointer t -> pointer_type (generate_llvm_type llctx t)
    | Array t -> (match array_size with
        | -1 -> pointer_type (generate_llvm_type llctx t)
        | _ -> array_type (generate_llvm_type llctx t) array_size)
;;

(* Unused at the moment *)
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

let is_array_type array_expr scopes : bool =
  match array_expr with
  | Id id_name ->
    let llv = lookup_value scopes id_name in
    let llt = type_of llv in
    (match classify_type llt with
     | TypeKind.Pointer ->
       let elt_t = element_type llt in
       (match classify_type elt_t with
        | TypeKind.Array -> true
        | _ -> false)
     | _ -> false)
  | _ -> raise (Failure "Parser should prevent this.")
;;

let rec generate_expression llctx fn llbuilder scopes expr load_value : llvalue =
  match expr with
  | Assignment (lhs, rhs) ->
    let lhs_value = generate_expression llctx fn llbuilder scopes lhs false in
    let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
    build_store rhs_value lhs_value llbuilder
  | LogicalOr (lhs, rhs) -> generate_logical_cmp llctx fn llbuilder scopes lhs rhs Or
  | LogicalAnd (lhs, rhs) -> generate_logical_cmp llctx fn llbuilder scopes lhs rhs And
  | LogicalNot expr ->
    let expr_v = generate_expression llctx fn llbuilder scopes expr false in
    build_xor expr_v (const_int (i1_type llctx) 1) "" llbuilder
  | Equal (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Eq
  | NotEqual (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Ne
  | LessThan (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Slt
  | LessThanEqual (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Sle
  | GreaterThan (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Sgt
  | GreaterThanEqual (lhs, rhs) -> generate_icmp llctx fn llbuilder scopes lhs rhs Icmp.Sge
  | Add (lhs, rhs) ->
    let lhs_value = generate_expression llctx fn llbuilder scopes lhs true in
    let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
    build_add lhs_value rhs_value "" llbuilder
  | Subtract (lhs, rhs) ->
    let lhs_value = generate_expression llctx fn llbuilder scopes lhs true in
    let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
    build_sub lhs_value rhs_value "" llbuilder
  | Multiply (lhs, rhs) ->
    let lhs_value = generate_expression llctx fn llbuilder scopes lhs true in
    let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
    build_mul lhs_value rhs_value "" llbuilder
  | Divide (lhs, rhs) ->
    let lhs_value = generate_expression llctx fn llbuilder scopes lhs true in
    let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
    build_sdiv lhs_value rhs_value "" llbuilder
  | Cast (_, _) -> todo "cast"
  | ArrayRef (array_expr, idx_expr) ->
    let is_array_t = is_array_type array_expr scopes in
    let idx_v = generate_expression llctx fn llbuilder scopes idx_expr true in
    let array_address = generate_expression llctx fn llbuilder scopes array_expr (not is_array_t) in
    let indices = (match is_array_t with
        | true -> [| (const_int (i32_type llctx) 0); idx_v |]
        | false -> [| idx_v |]) in
    let elt_ptr = build_in_bounds_gep array_address indices "" llbuilder in
    (match load_value with
     | true -> build_load elt_ptr "" llbuilder
     | false -> elt_ptr)
  | FunctionCall (fn_expr, arg_expr_list) ->
    let fn_v = generate_expression llctx fn llbuilder scopes fn_expr false in
    let args = Array.of_list_map arg_expr_list
        ~f:(fun arg_expr -> generate_expression llctx fn llbuilder scopes arg_expr true) in
    build_call fn_v args "" llbuilder
  | Id id_name ->
    let v_address = lookup_value scopes id_name in
    (match load_value with
     | false -> v_address
     | true ->
       (match is_array_type (Id id_name) scopes with
        | false -> build_load v_address "" llbuilder
        | true ->
          let zero = const_int (i32_type llctx) 0 in
          build_in_bounds_gep v_address [| zero; zero |] "" llbuilder))
  | IntConst n -> const_int (i32_type llctx) n
  | CharConst c -> const_int (i8_type llctx) (int_of_char c)
  | AddressOf expr -> generate_expression llctx fn llbuilder scopes expr false
  | Dereference expr ->
    let v_address = generate_expression llctx fn llbuilder scopes expr true in
    build_load v_address "" llbuilder
  | Negate expr ->
    let v = generate_expression llctx fn llbuilder scopes expr true in
    build_sub (const_int (i32_type llctx) 0) v "" llbuilder
and generate_icmp llctx fn llbuilder scopes lhs rhs (cmp : Icmp.t) : llvalue =
  let lhs_value = generate_expression llctx fn llbuilder scopes lhs true in
  let rhs_value = generate_expression llctx fn llbuilder scopes rhs true in
  build_icmp cmp lhs_value rhs_value "" llbuilder
and generate_logical_cmp llctx fn llbuilder scopes lhs rhs (cmp : logical_cmp) : llvalue =
  (* result_v is for the overall result of the OR, since I'm trying to avoid phi nodes *)
  let result_v = build_alloca (generate_llvm_type llctx Bool) "" llbuilder in

  (* Evaluate the LHS, and store the result *)
  let lhs_value = generate_expression llctx fn llbuilder scopes lhs false in
  let _ = build_store lhs_value result_v llbuilder in

  (* Start a new block and evaluate the RHS, storing the result *)
  let rhs_block = append_block llctx "" fn in
  let rhs_builder = builder_at_end llctx rhs_block in
  let rhs_value = generate_expression llctx fn rhs_builder scopes rhs false in
  let _ = build_store rhs_value result_v rhs_builder in

  (* Create the exit block for ongoing generation *)
  let cont_block = append_block llctx "" fn in

  (* Create a branch back in the original block *)
  let _ = match cmp with
    | Or -> build_cond_br lhs_value cont_block rhs_block llbuilder
    | And -> build_cond_br lhs_value rhs_block cont_block llbuilder in
  let _ = build_br cont_block rhs_builder in

  (* Move the original llbuilder down to the end of the cont_block *)
  let _ = position_at_end cont_block llbuilder in

  (* Return the result value *)
  result_v
;;

let add_function_to_scopes llctx llm scopes ret_type name arg_list : llvalue =
  (* Build function definition *)
  let llvm_ret_type = generate_llvm_type llctx ret_type in
  let llvm_arg_list = generate_llvm_arg_list llctx arg_list in
  let fn_type = function_type llvm_ret_type llvm_arg_list in
  let fn = define_function name fn_type llm in

  (* Register function in environment *)
  let _ = add_to_scope scopes name fn ret_type in fn
;;

let generate_declaration llctx llm (llbuilder : llbuilder option) scopes decl : unit =
  match decl with
  | VariableDeclaration (id_type, id_name) -> begin
      let id_llvm_type = generate_llvm_type llctx id_type in
      let id_llv = (match llbuilder with
          | None -> declare_global id_llvm_type id_name llm
          | Some llbuilder -> build_alloca id_llvm_type "" llbuilder) in
      add_to_scope scopes id_name id_llv id_type;
    end
  | ArrayDeclaration (id_type, id_name, array_size) -> begin
      let id_llvm_type = generate_llvm_type llctx id_type ~array_size:array_size in
      let id_llv = (match llbuilder with
          | None -> declare_global id_llvm_type id_name llm
          | Some llbuilder -> build_alloca id_llvm_type "" llbuilder) in
      add_to_scope scopes id_name id_llv id_type;
    end
  | FunctionDeclaration (ret_type, name, arg_list) ->
    let _ = add_function_to_scopes llctx llm scopes ret_type name arg_list in ()
;;

let rec generate_statement llctx llm
    (fn : llvalue)
    (ret_block : llbasicblock)
    (ret_v_addr : llvalue)
    (llbuilder : llbuilder)
    scopes stmt : unit =
  let _ = match stmt with
    | Block (decls, stmts) ->
      let _ = List.map decls ~f:(fun decl -> generate_declaration llctx llm (Some llbuilder) scopes decl) in
      let _ = List.map stmts ~f:(fun stmt -> generate_statement llctx llm fn ret_block ret_v_addr llbuilder scopes stmt) in ()
    | Conditional (test_expr, then_stmt, else_stmt_opt) ->
      (* Evaluate the test expression *)
      let result_v = generate_expression llctx fn llbuilder scopes test_expr false in

      (* Start a new block for the then branch *)
      let then_block = append_block llctx "" fn in
      let then_builder = builder_at_end llctx then_block in
      let _ = generate_statement llctx llm fn ret_block ret_v_addr then_builder scopes then_stmt in

      (* Start a new block for the else branch *)
      let else_block = append_block llctx "" fn in
      let _ = (match else_stmt_opt with
          | None -> ()
          | Some else_stmt ->
            let else_builder = builder_at_end llctx else_block in
            let _ = generate_statement llctx llm fn ret_block ret_v_addr else_builder scopes else_stmt in ()) in

      (* Branch based on the result *)
      let _ = build_cond_br result_v then_block else_block llbuilder in

      (* Create the exit block for ongoing computation *)
      let cont_block = append_block llctx "" fn in

      (* Update the llbuilder pointer *)
      let _ = position_at_end cont_block llbuilder in

      (* If both branches end with a terminator already, then we can delete the cont block. *)
      let then_terminates = match block_terminator then_block with
        | None -> false
        | Some _ -> true in
      let else_terminates = match block_terminator else_block with
        | None -> false
        | Some _ -> true in
      let _ = match then_terminates && else_terminates with
      | true -> delete_block cont_block
      | false -> () in ()
    | Expression expr ->
      let _ = generate_expression llctx fn llbuilder scopes expr true in ()
    | Loop _ -> todo "statement - Loop"
    | Return ret_expr -> (match ret_expr with
        | None -> let _ = build_br ret_block llbuilder in ()
        | Some expr ->
          let ret_value = generate_expression llctx fn llbuilder scopes expr true in
          let _ = build_store ret_value ret_v_addr llbuilder in
          let _ = build_br ret_block llbuilder in ())
    | StmtVoid -> () in ()
;;

let allocate_function_memory llctx llbuilder scopes
    (ret_type : subc_type)
    (fn : llvalue)
    (arg_list : arg_list) : llvalue =

  (* Allocate memory for return value *)
  let ret_v = match ret_type with
    | Void -> const_null (void_type llctx)
    | _ -> build_alloca (generate_llvm_type llctx ret_type) "" llbuilder in

  (* Allocations for function arguments *)
  let _ = match arg_list with
    | ArgVoid -> ()
    | ArgList args ->
      List.iter args ~f:(fun (arg_type, name) ->
          let lbl = build_alloca (generate_llvm_type llctx arg_type) "" llbuilder in
          add_to_scope scopes name lbl arg_type) in

  (* Stores *)
  let _ = match arg_list with
    | ArgVoid -> ()
    | ArgList args ->
      List.iteri args ~f:(fun i (_, name) ->
          let _ = build_store (param fn i) (lookup_value scopes name) llbuilder in ())

  in ret_v
;;

let generate_function_definition llctx llm scopes ret_type name arg_list stmt : unit =
  (* Get a handle to the LLVM function *)
  let fn = try lookup_value scopes name with
    | IdNotFound _ -> add_function_to_scopes llctx llm scopes ret_type name arg_list in

  (* Create builder for function blocks *)
  let llbuilder = builder_at_end llctx (entry_block fn) in

  (* Allocate memory for return value and each argument *)
  let ret_v_address = allocate_function_memory llctx llbuilder scopes ret_type fn arg_list in

  (* Create a block fo the return instruction *)
  let return_block = append_block llctx "return" fn in

  (* Generate blocks for function body *)
  let _ = generate_statement llctx llm fn return_block ret_v_address llbuilder scopes stmt in

  (* Make sure the return block comes last, just for aesthetic reasons *)
  let _ = move_block_after (Array.last (basic_blocks fn)) return_block in

  (* Build the return instruction in the return block *)
  let ret_builder = builder_at_end llctx return_block in
  let _ = match ret_type with
  | Void -> build_ret_void ret_builder
  | _ ->
    let ret_v = build_load ret_v_address "" ret_builder in
    build_ret ret_v ret_builder in

  (* Final cleanup step, make sure that all basic blocks have terminators. Any blocks that don't
     end with a terminator get an unconditional jump to the return block, unless they don't have
     any predecessors, in which case they're deleted *)
  Array.iter (basic_blocks fn)
    ~f:(fun bb -> match block_terminator bb with
        | None ->
          let builder = builder_at_end llctx bb in
          let _ = build_br return_block builder in ()
        | Some _ -> ())
;;

let generate_subc_unit llctx llm scopes (u : subc_unit) =
  match u with
  | Declaration decl ->
    generate_declaration llctx llm None scopes decl
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
  match Llvm_analysis.verify_module llm with
  | None -> llm
  | Some err ->
    printf "\n\nModule is not valid!\nError:\n%s\n\nModule:\n\n%s" err (string_of_llmodule llm);
    raise (Failure "Module is not valid!")
;;
