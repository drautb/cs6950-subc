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

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | _ ->
    printf "Parsed %d lines successfully!\n" lexbuf.lex_curr_p.pos_lnum

let loop filename () =
  printf "Parsing file: %s...\n\n" filename;
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Compiler for SubC"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
