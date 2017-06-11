open OUnit2
open Core

open Typechecker

let load_programs path =
  let base = (Sys.getcwd ()) ^ "/typechecker_examples/" ^ path ^ "/" in
  let file_list = Sys.readdir base in
  List.map (List.filter (Array.to_list file_list)
              ~f:(fun filename ->
                  (String.equal ".c"
                     (String.sub filename ~pos:((String.length filename) - 2) ~len:2))))
    ~f:(fun filename ->
        let file = In_channel.create (base ^ filename) in
        let contents = In_channel.input_all file in
        let test_case = (filename, contents) in
        In_channel.close file;
        test_case)
;;

(* Takes a thunk as an argument, raises an error if the thunk does _not_
   raise a Typechecker.TypeError *)
let assert_type_error name f =
  try f (); assert_failure "" with
    | TypeError _ -> ()
    | _ -> assert_failure (Printf.sprintf "Expected program '%s' to not typecheck!" name)
;;

let accepted_programs =
  List.append (load_programs "accept")
    [
      (* 'main' function related *)
      ("proper main without declaration",
       "int main(int argc, char* argv[]) { return 0; }");

      ("proper main with declaration",
       "int main(int argc, char* argv[]);" ^
       "int main(int argc, char* argv[]) { return 0; }");

      (* Variable declaration/scoping related checks *)
      ("top-level variable shadowed in nested scope",
       "int x;" ^
       "int main(int argc, char* argv[]) {" ^
       "  int x;" ^
       "  return 0;" ^
       "}");

      ("variable shadowed in nested scope",
       "int main(int argc, char* argv[]) {" ^
       "  int x;" ^
       "  { int x; }" ^
       "  return 0;" ^
       "}")
    ]

let rejected_programs =
  List.append (load_programs "reject")
    [
      (* 'main' function related *)
      ("missing main",
       "void fn(void) {}");

      ("main not defined",
       "int main(int argc, char* argv[]);");

      ("main has wrong return type",
       "void main(int argc, char* argv[]) { return 0; }");

      ("main has wrong argument list",
       "int main(void) { return 0; }");

      (* General function checks *)
      ("duplicate function declaration",
       "void fn(void);" ^
       "void fn(void);" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("duplicate function definition",
       "void fn(void) { return; }" ^
       "void fn(void) { return; }" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("function definition precedes declaration",
       "void fn(void) { return; }" ^
       "void fn(void);" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("function definition does not match declaration return type",
       "void fn(void);" ^
       "int fn(void) { return 0; }" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("function definition does not match declaration argument list",
       "void fn(void);" ^
       "void fn(int n) { return; }" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("function declaration duplicates formal argument names",
       "int main(int argc, char* argc[]);" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("function definition duplicates formal argument names",
       "int main(int argc, char* argv[]);" ^
       "int main(int argc, char* argc[]) { return 0; }");

      ("non-void function returns void",
       "int main(int argc, char* argv[]) { return 0; }" ^
       "int fn(void) { return; }");

      ("void function returns a value",
       "int main(int argc, char* argv[]) { return 0; }" ^
       "void fn(void) { return 1; }");

      (* Variable declaration/scope checks *)
      ("variable redefined at top-level",
       "int x;" ^
       "int x;" ^
       "int main(int argc, char* argv[]) { return 0; }");

      ("variable redefined in same scope",
       "int main(int argc, char* argv[]) {" ^
       "  int x;" ^
       "  int x;" ^
       "  return 0;" ^
       "}");

      (* Array size declaration check. (Parser prevents negative sizes) *)
      ("array declared with zero size",
       "int x[0];" ^
       "int main(int argc, char* argv[]) { return 0; }");
    ]

let typechecker_tests =
  "typechecker_tests" >:::
  (List.append
     (List.map accepted_programs
        ~f:(fun (name, prog) ->
            ("Accept: " ^ name) >::
            (fun _ -> typecheck (Subc.ast_of_string prog))))

     (List.map rejected_programs
        ~f:(fun (name, prog) ->
            ("Reject: " ^ name) >::
            (fun _ ->
               let f = fun () -> typecheck (Subc.ast_of_string prog) in
               (assert_type_error name f)))))
;;
