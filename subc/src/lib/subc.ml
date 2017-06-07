let ast_of_string prog_str =
  Parser.program Lexer.read (Lexing.from_string prog_str)
;;
