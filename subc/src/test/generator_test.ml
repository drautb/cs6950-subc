open Core
open OUnit2
open Printf
open Re2

let examples = [

  ("simple main fn",
   "int main(int argc, char* argv[]) {
      return 0;
    }",
   "define i32 @main() {
    entry:
      ret i32 0
    }");




]

(* Does some string munging to make the example match reality
   in terms of spacing/formatting. *)
let build_expected (name : string) (expected_str : string) : string =
  let re = Regex.create_exn "\n    " in
  let reindented_str =  Regex.rewrite_exn re ~template:"\n" expected_str in
  let header = sprintf "; ModuleID = '%s'\nsource_filename = \"%s\"\n\n" name name in
  header ^ reindented_str
;;

let generator_tests =
  "generator_tests" >:::
  (List.map examples
     ~f:(fun (name, program, ll) ->
         name >::
         (fun _ ->
            let actual_module = (Llvm.string_of_llmodule (Subc.llmodule_of_program name program)) in
            assert_equal
              (String.strip (build_expected name ll))
              (String.strip actual_module)
              ~msg:(sprintf
                      ("Expected modules to be equal!\nExpected:
------------------------------------------
%s
------------------------------------------
Actual:
------------------------------------------
%s
------------------------------------------")
                      ll actual_module))))
;;
