open Core
open OUnit2
open Printf
open Re2

let examples = [

  (* ---------------------------------------------------- *)
  ("simple main fn",

   "int main(int argc, char* argv[]) {
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i8**, align 8
      store i32 0, i32* %2, align 4
      store i32 %0, i32* %3, align 4
      store i8** %1, i8*** %4, align 8
      ret i32 0
    }");

  (* ---------------------------------------------------- *)
  ("function with void return type",

   "int main(int argc, char* argv[]) {
      return 0;
    }
    void fn(void) {
      return;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i8**, align 8
      store i32 0, i32* %2, align 4
      store i32 %0, i32* %3, align 4
      store i8** %1, i8*** %4, align 8
      ret i32 0
    }

    define void @fn() {
    entry:
      ret void
    }");

  (* ---------------------------------------------------- *)
  ("function with char return type",

   "int main(int argc, char* argv[]) {
      return 0;
    }
    char fn(void) {
      return 'c';
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i8**, align 8
      store i32 0, i32* %2, align 4
      store i32 %0, i32* %3, align 4
      store i8** %1, i8*** %4, align 8
      ret i32 0
    }

    define i8 @fn() {
    entry:
      %0 = alloca i8, align 1
      store i8 0, i8* %0, align 1
      ret i8 99
    }");

  (* ---------------------------------------------------- *)
  ("function with pointer return type",

   "int main(int argc, char* argv[]) {
      return 0;
    }
    int* fn(void) {
      int n;
      return &n;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i8**, align 8
      store i32 0, i32* %2, align 4
      store i32 %0, i32* %3, align 4
      store i8** %1, i8*** %4, align 8
      ret i32 0
    }

    define i32* @fn() {
    entry:
      %0 = alloca i32*, align 8
      %1 = alloca i32, align 4
      ret i32* %1
    }");

  (* ---------------------------------------------------- *)
  ("function with array parameter",

   "int main(int argc, char* argv[]) {
      return 0;
    }
    void fn(int stuff[]) {
      return;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i32, align 4
      %4 = alloca i8**, align 8
      store i32 0, i32* %2, align 4
      store i32 %0, i32* %3, align 4
      store i8** %1, i8*** %4, align 8
      ret i32 0
    }

    define void @fn(i32*) {
    entry:
      %1 = alloca i32*, align 8
      store i32* %0, i32** %1, align 8
      ret void
    }");

  (* ---------------------------------------------------- *)
(*
  ("simple integer assignment",

   "int main(int argc, char* argv[]) {
      int n;
      n = 42;
      return n;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      ret i32 0
    }");
*)

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
            let expected_module = String.strip (build_expected name ll) in
            let actual_module = String.strip (Llvm.string_of_llmodule (Subc.llmodule_of_program name program)) in
            assert_equal expected_module actual_module
              ~msg:(sprintf ("Expected modules to be equal!\nExpected:
------------------------------------------
%s
------------------------------------------
Actual:
------------------------------------------
%s
------------------------------------------")
                      ll actual_module))))
;;
