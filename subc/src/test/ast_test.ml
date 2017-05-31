open OUnit2
open Core

let load_programs =
  let base = (Sys.getcwd ()) ^ "/ast_examples/" in
  let file_list = Sys.readdir base in
  List.map (List.filter (Array.to_list file_list)
              ~f:(fun filename ->
                  (String.equal ".c"
                     (String.sub filename ~pos:((String.length filename) - 2) ~len:2))))
    ~f:(fun filename ->
        let file = In_channel.create (base ^ filename) in
        let contents = In_channel.input_all file in
        let prog_and_ast = String.split contents ~on:'@' in
        let test_case = (filename, List.nth_exn prog_and_ast 0, List.nth_exn prog_and_ast 1) in
        In_channel.close file;
        test_case)
;;

let parse_program prog_str =
  Parser.program Lexer.read (Lexing.from_string prog_str)
;;

let programs = load_programs

let ast_tests =
  "ast_tests" >:::
  (List.map programs
     ~f:(fun (name, prog, ast_text) ->
         name >::
         (fun _ ->
            let expected_ast = List.t_of_sexp Ast.subc_unit_of_sexp
                (Sexp.of_string ast_text) in
            let actual_ast = parse_program prog in
            assert_equal expected_ast actual_ast)))
;;
