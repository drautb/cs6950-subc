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
          try Ast (List.t_of_sexp subc_unit_of_sexp
                     (Sexp.of_string (List.nth_exn prog_and_ast 1)))
          with Sexp.Parse_error _ -> (eprintf "\nAn error occurred while reading %s\n" filename); Ast [] in
        let test_case = (filename, prog_str, expected_ast) in
        In_channel.close file;
        test_case)
;;

let embedded_tests = [
  (* -----------------------------------------------------------------
     Variable/Array Declarations *)
  ("basic int variable declaration",
   "int x;",
   Ast [Declaration (VariableDeclaration (Int, "x"))]);

  ("basic char variable declaration",
   "char c;",
   Ast [Declaration (VariableDeclaration (Char, "c"))]);

  ("basic int pointer variable declaration",
   "int* x;",
   Ast [Declaration (VariableDeclaration ((Pointer Int), "x"))]);

  ("multi-level pointer to variable declaration",
   "int*** triple;",
   Ast [Declaration (VariableDeclaration ((Pointer (Pointer (Pointer Int))), "triple"))]);

  ("basic int array declaration",
   "int arr[5];",
   Ast [Declaration (ArrayDeclaration (Int, "arr", 5))]);

  ("int pointer array declaration",
   "int* arr[10];",
   Ast [Declaration (ArrayDeclaration ((Pointer Int), "arr", 10))]);

  ("multi-level char pointer array declaration",
   "char*** letters[256];",
   Ast [Declaration (ArrayDeclaration ((Pointer (Pointer (Pointer Char))), "letters", 256))]);

  (* -----------------------------------------------------------------
     Function Declarations *)
  ("simplest function declaration",
   "void empty(void);",
   Ast [Declaration (FunctionDeclaration (Void, "empty", ArgVoid))]);

  ("main function",
   "int main(int argc, char* argv[]);",
   Ast [Declaration
          (FunctionDeclaration
             (Int, "main", (ArgList [(Int, "argc", false);
                                     ((Pointer Char), "argv", true)])))])
];;

let ast_tests =
  "ast_tests" >:::
  (List.map (List.append load_external_tests embedded_tests)
     ~f:(fun (name, prog_str, expected_ast) ->
         name >::
         (fun _ ->
            let actual_ast = Subc.ast_of_string prog_str in
            assert_equal
              expected_ast actual_ast
              ~msg:(sprintf
                      "Program did not produce expected AST!\nSource:\n\n%s\n\nActual AST:\n\n%s\n\nExpected AST:\n\n%s\n\n"
                      prog_str (string_of_ast actual_ast) (string_of_ast expected_ast)))))
;;
