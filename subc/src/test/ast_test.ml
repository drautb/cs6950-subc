open OUnit2
open Core

open Ast

let load_external_tests =
  let base = (Sys.getcwd ()) ^ "/ast_examples/" in
  let file_list = Sys.readdir base in
  List.map (List.filter (Core.Array.to_list file_list)
              ~f:(fun filename ->
                  (String.equal ".c"
                     (String.sub filename ~pos:((String.length filename) - 2) ~len:2))))
    ~f:(fun filename ->
        let file = In_channel.create (base ^ filename) in
        let contents = In_channel.input_all file in
        let prog_and_ast = String.split contents ~on:'@' in
        let prog_str = List.nth_exn prog_and_ast 0 in
        let expected_ast =
          try List.t_of_sexp subc_unit_of_sexp
                (Sexp.of_string (List.nth_exn prog_and_ast 1))
          with Sexp.Parse_error _ -> (eprintf "\nAn error occurred while reading %s\n" filename); [] in
        let test_case = (filename, prog_str, expected_ast) in
        In_channel.close file;
        test_case)
;;

let embedded_tests = [
  ("basic int variable declaration",
   "int x;",
   [Declaration (Variable {Variable.var_type = Int; Variable.id = "x"})]);

  ("basic char variable declaration",
   "char c;",
   [Declaration (Variable {Variable.var_type = Char; Variable.id = "c"})]);

  ("basic int pointer variable declaration",
   "int* x;",
   [Declaration (Variable {Variable.var_type = Pointer Int; Variable.id = "x"})]);

  ("multi-level pointer to variable declaration",
   "int*** triple;",
   [Declaration (Variable {Variable.var_type = Pointer (Pointer (Pointer Int));
                           Variable.id = "triple"})]);

  ("basic int array declaration",
   "int arr[5];",
   [Declaration (Array {Array.var_type = Int; Array.id = "arr"; Array.size = 5})]);

  ("int pointer array declaration",
   "int* arr[10];",
   [Declaration (Array {Array.var_type = Pointer Int; Array.id = "arr"; Array.size = 10})]);

  ("multi-level char pointer array declaration",
   "char*** letters[256];",
   [Declaration (Array {Array.var_type = Pointer (Pointer (Pointer Char));
                        Array.id = "letters";
                        Array.size = 256})])
];;

let parse_program prog_str =
  Parser.program Lexer.read (Lexing.from_string prog_str)
;;

let string_of_ast ast =
  List.to_string ~f:(fun elt -> Sexp.to_string (sexp_of_subc_unit elt)) ast
;;

let ast_tests =
  "ast_tests" >:::
  (List.map (List.append load_external_tests embedded_tests)
     ~f:(fun (name, prog_str, expected_ast) ->
         name >::
         (fun _ ->
            let actual_ast = parse_program prog_str in
            assert_equal
              expected_ast actual_ast
              ~msg:(sprintf
                      "Program did not produce expected AST!\nSource:\n\n%s\n\nActual AST:\n\n%s\n\nExpected AST:\n\n%s\n\n"
                      prog_str (string_of_ast actual_ast) (string_of_ast expected_ast)))))
;;
