open Core
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit(-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit(-2)

let parse lexbuf =
  match parse_with_error lexbuf with
  | ast -> ast
;;

let get_program_name filename =
  Filename.chop_extension (Filename.basename filename)
;;

let save_ast ast filename =
  Out_channel.write_all ((get_program_name filename) ^ ".ast")
    ~data:(Ast.string_of_ast ast)
;;

let save_llvm llvm filename =
  let program_name = get_program_name filename in
  match Llvm_bitwriter.write_bitcode_file llvm (program_name ^ ".bc") with
  | true -> ()
  | false -> raise (Failure "Failed to write LLVM .bc file")
;;

let run emit_ast emit_llvm filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = parse lexbuf in
  let _ = match emit_ast with
    | true -> save_ast ast filename
    | false -> () in
  let _ = Typechecker.typecheck ast in
  let llvm_module = Generator.generate_module (get_program_name filename) ast in
  let _ = match emit_llvm with
    | true -> save_llvm llvm_module filename
    | false -> () in
  In_channel.close inx

let () =
  Command.basic
    ~summary:"Compiler for SubC programs"
    Command.Spec.(empty
                  +> flag "--emit-ast" no_arg ~doc:"Produce an .ast file representing the program."
                  +> flag "--emit-llvm" no_arg ~doc:"Produce an LLVM .bc file representing the program."
                  +> anon ("filename" %: file))
    run
  |> Command.run
