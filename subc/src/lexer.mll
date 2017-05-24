{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let charcon = '\'' [' '-'&' '('-'[' ']'-'~'] '\''
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = letter (letter|digit|'_')*
let intcon = digit+

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | intcon   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | charcon  { CHAR (Lexing.lexeme lexbuf).[0] }
  | "void"   { VOID_LIT }
  | "char"   { CHAR_LIT }
  | "int"    { INT_LIT }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "return" { RETURN }
  | id       { ID (Lexing.lexeme lexbuf) }
  | "=="     { B_EQ }
  | "!="     { B_NEQ }
  | "<="     { B_LEQ }
  | "<"      { B_LESS }
  | ">="     { B_GEQ }
  | ">"      { B_GREATER }
  | "&&"     { L_AND }
  | "||"     { L_OR }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACKET }
  | ']'      { RIGHT_BRACKET }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '*'      { STAR }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '/'      { SLASH }
  | '&'      { AMP }
  | '='      { ASSIGN }
  | '!'      { L_NOT }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
