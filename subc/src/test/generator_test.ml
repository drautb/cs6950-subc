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
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
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
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
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
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
      ret i32 0
    }

    define i8 @fn() {
    entry:
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
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
      ret i32 0
    }

    define i32* @fn() {
    entry:
      %0 = alloca i32, align 4
      ret i32* %0
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
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
      ret i32 0
    }

    define void @fn(i32*) {
    entry:
      %1 = alloca i32*, align 8
      store i32* %0, i32** %1, align 8
      ret void
    }");

  (* ---------------------------------------------------- *)
  ("simple integer assignment",

   "int main(int argc, char* argv[]) {
      int n;
      n = 42;
      return n;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
      %4 = alloca i32, align 4
      store i32 42, i32* %4, align 4
      %5 = load i32, i32* %4, align 4
      ret i32 %5
    }");

  (* ---------------------------------------------------- *)
  ("simple pointer assignment",

   "int main(int argc, char* argv[]) {
      int* p1;
      int* p2;
      p1 = &argc;
      p2 = p1;
      return *p2;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32, align 4
      %3 = alloca i8**, align 8
      store i32 %0, i32* %2, align 4
      store i8** %1, i8*** %3, align 8
      %4 = alloca i32*, align 8
      %5 = alloca i32*, align 8
      store i32* %2, i32** %4, align 4
      %6 = load i32*, i32** %4, align 8
      store i32* %6, i32** %5, align 4
      %7 = load i32*, i32** %5, align 8
      %8 = load i32, i32* %7
      ret i32 %8
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