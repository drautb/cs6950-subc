let ast_of_string prog_str =
  Parser.program Lexer.read (Lexing.from_string prog_str)
;;

let llmodule_of_program module_name prog_str =
  let ast = ast_of_string prog_str in
  let _ = Typechecker.typecheck ast in
  Generator.generate_module module_name ast
;;
