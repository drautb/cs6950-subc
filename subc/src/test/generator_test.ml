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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %5 = load i32, i32* %2
      ret i32 %5
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %5 = load i32, i32* %2
      ret i32 %5
    }

    define void @fn() {
    entry:
      br label %return

    return:                                           ; preds = %entry
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %5 = load i32, i32* %2
      ret i32 %5
    }

    define i8 @fn() {
    entry:
      %0 = alloca i8
      store i8 99, i8* %0
      br label %return

    return:                                           ; preds = %entry
      %1 = load i8, i8* %0
      ret i8 %1
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %5 = load i32, i32* %2
      ret i32 %5
    }

    define i32* @fn() {
    entry:
      %0 = alloca i32*
      %1 = alloca i32
      store i32* %1, i32** %0
      br label %return

    return:                                           ; preds = %entry
      %2 = load i32*, i32** %0
      ret i32* %2
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %5 = load i32, i32* %2
      ret i32 %5
    }

    define void @fn(i32*) {
    entry:
      %1 = alloca i32*
      store i32* %0, i32** %1
      br label %return

    return:                                           ; preds = %entry
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      store i32 42, i32* %5
      %6 = load i32, i32* %5
      store i32 %6, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %7 = load i32, i32* %2
      ret i32 %7
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32*
      %6 = alloca i32*
      store i32* %3, i32** %5
      %7 = load i32*, i32** %5
      store i32* %7, i32** %6
      %8 = load i32*, i32** %6
      %9 = load i32, i32* %8
      store i32 %9, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %10 = load i32, i32* %2
      ret i32 %10
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      %7 = alloca i32
      %8 = alloca i32
      store i32 10, i32* %6
      store i32 5, i32* %7
      store i32 3, i32* %8
      %9 = load i32, i32* %6
      %10 = load i32, i32* %7
      %11 = sub i32 %9, %10
      %12 = load i32, i32* %8
      %13 = mul i32 %11, %12
      %14 = add i32 2, %13
      store i32 %14, i32* %5
      %15 = load i32, i32* %5
      store i32 %15, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %16 = load i32, i32* %2
      ret i32 %16
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      %7 = alloca i32
      store i32 10, i32* %6
      store i32 5, i32* %7
      %8 = load i32, i32* %6
      %9 = load i32, i32* %7
      %10 = sdiv i32 %8, %9
      store i32 %10, i32* %5
      %11 = load i32, i32* %5
      store i32 %11, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %12 = load i32, i32* %2
      ret i32 %12
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      store i32 10, i32* %5
      %6 = load i32, i32* %5
      %7 = sub i32 0, %6
      store i32 %7, i32* %5
      %8 = load i32, i32* %5
      store i32 %8, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %9 = load i32, i32* %2
      ret i32 %9
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      store i32 0, i32* @n
      %5 = load i32, i32* @n
      store i32 %5, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %6 = load i32, i32* %2
      ret i32 %6
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i8*
      %6 = load i8**, i8*** %4
      %7 = getelementptr inbounds i8*, i8** %6, i32 3
      %8 = load i8*, i8** %7
      store i8* %8, i8** %5
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %9 = load i32, i32* %2
      ret i32 %9
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
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i8*
      %6 = alloca i32
      store i32 4, i32* %6
      %7 = load i32, i32* %6
      %8 = add i32 %7, 3
      %9 = load i8**, i8*** %4
      %10 = getelementptr inbounds i8*, i8** %9, i32 %8
      %11 = load i8*, i8** %10
      store i8* %11, i8** %5
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %12 = load i32, i32* %2
      ret i32 %12
    }");

  (* ---------------------------------------------------- *)
  ("read-only array reference for a declared array",

   "int arr[10];
    int main(int argc, char* argv[]) {
      int n;
      n = 5;
      return arr[n];
    }",

   "@arr = external global [10 x i32]

    define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      store i32 5, i32* %5
      %6 = load i32, i32* %5
      %7 = getelementptr inbounds [10 x i32], [10 x i32]* @arr, i32 0, i32 %6
      %8 = load i32, i32* %7
      store i32 %8, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %9 = load i32, i32* %2
      ret i32 %9
    }");

  (* ---------------------------------------------------- *)
  ("array reference assignment for local array",

   "int main(int argc, char* argv[]) {
      int arr[5];
      arr[3] = 42;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca [5 x i32]
      %6 = getelementptr inbounds [5 x i32], [5 x i32]* %5, i32 0, i32 3
      store i32 42, i32* %6
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %7 = load i32, i32* %2
      ret i32 %7
    }");

  (* ---------------------------------------------------- *)
  ("array reference read and assignment for function parameter array",

   "int fn(int a[]) {
      int x;
      x = a[2];
      a[4] = 42;
      return a[4];
    }

    int main(int argc, char* argv[]) {
      int arr[5];
      return fn(arr);
    }",

   "define i32 @fn(i32*) {
    entry:
      %1 = alloca i32
      %2 = alloca i32*
      store i32* %0, i32** %2
      %3 = alloca i32
      %4 = load i32*, i32** %2
      %5 = getelementptr inbounds i32, i32* %4, i32 2
      %6 = load i32, i32* %5
      store i32 %6, i32* %3
      %7 = load i32*, i32** %2
      %8 = getelementptr inbounds i32, i32* %7, i32 4
      store i32 42, i32* %8
      %9 = load i32*, i32** %2
      %10 = getelementptr inbounds i32, i32* %9, i32 4
      %11 = load i32, i32* %10
      store i32 %11, i32* %1
      br label %return

    return:                                           ; preds = %entry
      %12 = load i32, i32* %1
      ret i32 %12
    }

    define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca [5 x i32]
      %6 = getelementptr inbounds [5 x i32], [5 x i32]* %5, i32 0, i32 0
      %7 = call i32 @fn(i32* %6)
      store i32 %7, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %8 = load i32, i32* %2
      ret i32 %8
    }");

  (* ---------------------------------------------------- *)
  ("equal comparison",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      x == y;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = load i32, i32* %5
      %8 = load i32, i32* %6
      %9 = icmp eq i32 %7, %8
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %10 = load i32, i32* %2
      ret i32 %10
    }");

  (* ---------------------------------------------------- *)
  ("not equal comparison",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      x != y;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = load i32, i32* %5
      %8 = load i32, i32* %6
      %9 = icmp ne i32 %7, %8
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %10 = load i32, i32* %2
      ret i32 %10
    }");

  (* ---------------------------------------------------- *)
  ("other integer comparisons",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      x < y;
      x <= y;
      x > y;
      x >= y;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = load i32, i32* %5
      %8 = load i32, i32* %6
      %9 = icmp slt i32 %7, %8
      %10 = load i32, i32* %5
      %11 = load i32, i32* %6
      %12 = icmp sle i32 %10, %11
      %13 = load i32, i32* %5
      %14 = load i32, i32* %6
      %15 = icmp sgt i32 %13, %14
      %16 = load i32, i32* %5
      %17 = load i32, i32* %6
      %18 = icmp sge i32 %16, %17
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %19 = load i32, i32* %2
      ret i32 %19
    }");

  (* ---------------------------------------------------- *)
  ("logical not",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      !(x == y);
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = load i32, i32* %5
      %8 = load i32, i32* %6
      %9 = icmp eq i32 %7, %8
      %10 = xor i1 %9, true
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %11 = load i32, i32* %2
      ret i32 %11
    }");

  (* ---------------------------------------------------- *)
  ("logical or",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      x == y || x == y;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = alloca i1
      %8 = load i32, i32* %5
      %9 = load i32, i32* %6
      %10 = icmp eq i32 %8, %9
      store i1 %10, i1* %7
      br i1 %10, label %15, label %11

    ; <label>:11:                                     ; preds = %entry
      %12 = load i32, i32* %5
      %13 = load i32, i32* %6
      %14 = icmp eq i32 %12, %13
      store i1 %14, i1* %7
      br label %15

    ; <label>:15:                                     ; preds = %11, %entry
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %15
      %16 = load i32, i32* %2
      ret i32 %16
    }");

  (* ---------------------------------------------------- *)
  ("logical and",

   "int main(int argc, char* argv[]) {
      int x;
      int y;
      x = 3;
      y = 4;
      x == y && x == y;
      return 0;
    }",

   "define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      %6 = alloca i32
      store i32 3, i32* %5
      store i32 4, i32* %6
      %7 = alloca i1
      %8 = load i32, i32* %5
      %9 = load i32, i32* %6
      %10 = icmp eq i32 %8, %9
      store i1 %10, i1* %7
      br i1 %10, label %11, label %15

    ; <label>:11:                                     ; preds = %entry
      %12 = load i32, i32* %5
      %13 = load i32, i32* %6
      %14 = icmp eq i32 %12, %13
      store i1 %14, i1* %7
      br label %15

    ; <label>:15:                                     ; preds = %11, %entry
      store i32 0, i32* %2
      br label %return

    return:                                           ; preds = %15
      %16 = load i32, i32* %2
      ret i32 %16
    }");

  (* ---------------------------------------------------- *)
  ("function call with prototype",

   "int fn(int n);

    int main(int argc, char* argv[]) {
      int x;
      x = 0;
      return fn(x);
    }

    int fn(int n) {
      return n;
    }",

   "define i32 @fn(i32) {
    entry:
      %1 = alloca i32
      %2 = alloca i32
      store i32 %0, i32* %2
      %3 = load i32, i32* %2
      store i32 %3, i32* %1
      br label %return

    return:                                           ; preds = %entry
      %4 = load i32, i32* %1
      ret i32 %4
    }

    define i32 @main(i32, i8**) {
    entry:
      %2 = alloca i32
      %3 = alloca i32
      %4 = alloca i8**
      store i32 %0, i32* %3
      store i8** %1, i8*** %4
      %5 = alloca i32
      store i32 0, i32* %5
      %6 = load i32, i32* %5
      %7 = call i32 @fn(i32 %6)
      store i32 %7, i32* %2
      br label %return

    return:                                           ; preds = %entry
      %8 = load i32, i32* %2
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
