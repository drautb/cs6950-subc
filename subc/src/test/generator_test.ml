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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      ret i32 0
    }

    define i32* @fn() {
    entry:
      %0 = alloca i32
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      ret i32 0
    }

    define void @fn(i32*) {
    entry:
      %1 = alloca i32*
      store i32* %0, i32** %1
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i32
      store i32 42, i32* %4
      %5 = load i32, i32* %4
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
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i32*
      %5 = alloca i32*
      store i32* %2, i32** %4
      %6 = load i32*, i32** %4
      store i32* %6, i32** %5
      %7 = load i32*, i32** %5
      %8 = load i32, i32* %7
      ret i32 %8
    }");

  (* ---------------------------------------------------- *)
  ("arithmetic",

   "int main(int argc, char* argv[]) {
      int result;
      int x;
      int y;
      int z;
      x = 10;
      y = 5;
      z = 3;
      result = 2 + (x - y) * z;
      return result;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i32
      %5 = alloca i32
      %6 = alloca i32
      %7 = alloca i32
      store i32 10, i32* %5
      store i32 5, i32* %6
      store i32 3, i32* %7
      %8 = load i32, i32* %5
      %9 = load i32, i32* %6
      %10 = sub i32 %8, %9
      %11 = load i32, i32* %7
      %12 = mul i32 %10, %11
      %13 = add i32 2, %12
      store i32 %13, i32* %4
      %14 = load i32, i32* %4
      ret i32 %14
    }");

  (* ---------------------------------------------------- *)
  ("arithmetic with division",

   "int main(int argc, char* argv[]) {
      int result;
      int x;
      int y;
      x = 10;
      y = 5;
      result = x / y;
      return result;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i32
      %5 = alloca i32
      %6 = alloca i32
      store i32 10, i32* %5
      store i32 5, i32* %6
      %7 = load i32, i32* %5
      %8 = load i32, i32* %6
      %9 = sdiv i32 %7, %8
      store i32 %9, i32* %4
      %10 = load i32, i32* %4
      ret i32 %10
    }");

  (* ---------------------------------------------------- *)
  ("integer negation with re-assignment",

   "int main(int argc, char* argv[]) {
      int x;
      x = 10;
      x = -x;
      return x;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i32
      store i32 10, i32* %4
      %5 = load i32, i32* %4
      %6 = sub i32 0, %5
      store i32 %6, i32* %4
      %7 = load i32, i32* %4
      ret i32 %7
    }");

  (* ---------------------------------------------------- *)
  ("global declarations",

   "int n;
    int *pn;
    int arr[5];
    char c;

    int main(int argc, char* argv[]) {
      n = 0;
      return n;
    }",

   "@n = external global i32
    @pn = external global i32*
    @arr = external global [5 x i32]
    @c = external global i8

    define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      store i32 0, i32* @n
      %4 = load i32, i32* @n
      ret i32 %4
    }");

  (* ---------------------------------------------------- *)
  ("read-only array reference",

   "int main(int argc, char* argv[]) {
      char* c;
      c = argv[3];
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i8*
      %5 = load i8**, i8*** %3
      %6 = getelementptr inbounds i8*, i8** %5, i32 3
      %7 = load i8*, i8** %6
      store i8* %7, i8** %4
      ret i32 0
    }");

  (* ---------------------------------------------------- *)
  ("read-only array reference with arithmetic",

   "int main(int argc, char* argv[]) {
      char* c;
      int n;
      n = 4;
      c = argv[n + 3];
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i8**
      store i32 %0, i32* %2
      store i8** %1, i8*** %3
      %4 = alloca i8*
      %5 = alloca i32
      store i32 4, i32* %5
      %6 = load i32, i32* %5
      %7 = add i32 %6, 3
      %8 = load i8**, i8*** %3
      %9 = getelementptr inbounds i8*, i8** %8, i32 %7
      %10 = load i8*, i8** %9
      store i8* %10, i8** %4
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
