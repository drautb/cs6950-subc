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
      (* 'main' function related checks *)
      ("proper main without declaration",
       "int main(int argc, char* argv[]) { return 0; }");

      ("proper main with declaration",
       "int main(int argc, char* argv[]);
        int main(int argc, char* argv[]) { return 0; }");

      (* Function related checks *)
      ("void function with no return statement",
       "int main(int argc, char* argv[]) { return 0; }
        void fn(void) { }");

      (* Variable declaration/scoping related checks *)
      ("top-level variable shadowed in nested scope",
       "int x;
        int main(int argc, char* argv[]) {
          int x;
          return 0;
        }");

      ("variable shadowed in nested scope",
       "int main(int argc, char* argv[]) {
          int x;
          { int x; }
          return 0;
        }");

      ("function argument shadowed in nested scope",
       "int main(int argc, char* argv[]) {
          { int argc; }
          return 0;
        }");

      (* Statement related checks *)
      ("conditional test expression is boolean",
       "int main(int argc, char* argv[]) {
          if (argc == 5) {
            return 1;
          }
          return 0;
        }");

      (* Expression related checks *)
      ("assignment to int variable",
       "int main(int argc, char* argv[]) {
          int x;
          x = 5;
          return 0;
        }");

      ("assignment to char variable",
       "int main(int argc, char* arg[]) {
          char c;
          c = 'x';
          return 0;
        }");

      ("assignment to array index",
       "int main(int argc, char* argv[]) {
          int arr[5];
          arr[3] = 42;
          return 0;
        }");

      ("assignment to char pointer",
       "int main(int argc, char* argv[]) {
          char c;
          char* pc;
          c = 'z';
          pc = &c;
          *pc = 'y';
          return 0;
        }");

      ("logical operators",
       "int main(int argc, char* argv[]) {
          if (!(1 == 1) || 2 != 2 && 3 == 3) {
            return 0;
          }
          return 1;
        }");

      ("equal and not-equal int compatibility",
       "int main(int argc, char* argv[]) {
          char c;
          char d;
          c == d;
          c == 42;
          42 == d;
          42 == 42;
          c != d;
          c != 42;
          42 != d;
          42 != 42;
          return 0;
        }");

      ("comparison operator int compatibility",
       "int main(int argc, char* argv[]) {
          char c;
          char d;
          c < d; 0 < 0; c < 0; 0 < c;
          c <= d; 0 <= 0; c <= 0; 0 <= c;
          c > d; 0 > 0; d > 0; 0 > d;
          c >= d; 0 >= 0; c >= 0; 0 >= d;
          return 0;
        }");

      ("arithmetic operator int compatibility",
       "int main(int argc, char* argv[]) {
          char c;
          char d;
          c + d; 0 + 0; c + 0; 0 + c;
          c - d; 0 - 0; d - 0; 0 - d;
          c * d; 0 * 0; c * 0; 0 * c;
          c / d; 0 / 0; d / 0; 0 / d;
          return 0;
        }");

      ("casting pointers",
       "int main(int argc, char* argv[]) {
          int n;
          int* pn;
          char* pcn;
          n = 5;
          pn = &n;
          pcn = (char*)pn;
          return 0;
        }");

      ("casting arrays",
       "int main(int argc, char* argv[]) {
          int arr[5];
          char* pc;
          pc = (char*)arr;
          return 0;
        }");

      ("array reference",
       "int main(int argc, char* argv[]) {
          int arr[50];
          int i;
          char c;
          i = arr[2];
          c = arr[17];
          return 0;
        }");

      ("function call",
       "char my_fn(int n, int* pn, int array[], char character) {
          array[0] = n + *pn + character;
          return array[0];
        }

        int main(int argc, char* argv[]) {
          int x;
          int* px;
          int arr[50];
          char c;
          x = 2;
          px = &x;
          c = 'c';
          x = my_fn(x, px, arr, c);
          return 0;
        }");

      ("numeric negation",
       "int main(int argc, char* argv[]) {
          int n;
          int c;
          -n == -c;
          return 0;
        }");

      ("fibonacci function",
       "int fib(int n);

        int fib(int n) {
          if (n < 2) {
            return n;
          }

          return fib(n - 2) + fib(n - 1);
        }

        int main(int argc, char* argv[]) {
          return fib(10);
        }");

      ("fibonacci function - tail recursive",
       "int fib(int n, int current, int next);

        int fib(int n, int current, int next) {
          if (n == 0) {
            return current;
          }
          else {
            return fib(n - 1, next, current + next);
          }
        }

        int main(int argc, char* argv[]) {
          return fib(10, 0, 1);
        }");
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
       "void fn(void);
        void fn(void);
        int main(int argc, char* argv[]) { return 0; }");

      ("duplicate function definition",
       "void fn(void) { return; }
        void fn(void) { return; }
        int main(int argc, char* argv[]) { return 0; }");

      ("function definition precedes declaration",
       "void fn(void) { return; }
        void fn(void);
        int main(int argc, char* argv[]) { return 0; }");

      ("function definition does not match declaration return type",
       "void fn(void);
        int fn(void) { return 0; }
        int main(int argc, char* argv[]) { return 0; }");

      ("function definition does not match declaration argument list",
       "void fn(void);
        void fn(int n) { return; }
        int main(int argc, char* argv[]) { return 0; }");

      ("function declaration duplicates formal argument names",
       "int main(int argc, char* argc[]);
        int main(int argc, char* argv[]) { return 0; }");

      ("function definition duplicates formal argument names",
       "int main(int argc, char* argv[]);
        int main(int argc, char* argc[]) { return 0; }");

      ("non-void function returns void",
       "int main(int argc, char* argv[]) { return 0; }
        int fn(void) { return; }");

      ("non-void function lacks return statement",
       "int main(int argc, char* argv[]) { return 0; }
        int* fn(void) { }");

      ("void function returns a value",
       "int main(int argc, char* argv[]) { return 0; }
        void fn(void) { return 1; }");

      (* Variable declaration/scope checks *)
      ("variable redefined at top-level",
       "int x;
        int x;
        int main(int argc, char* argv[]) { return 0; }");

      ("variable redefined in same scope",
       "int main(int argc, char* argv[]) {
          int x;
          int x;
          return 0;
        }");

      ("function argument redefined in first scope of definition",
       "int main(int argc, char* argv[]) {
          int argc;
          return 0;
        }");

      (* Array size declaration check. (Parser prevents negative sizes) *)
      ("array declared with zero size",
       "int x[0];
        int main(int argc, char* argv[]) { return 0; }");

      (* Statement related checks *)
      ("conditional with non-bool test expression",
       "int main(int argc, char* argv[]) {
          if (5) {
            return 5;
          }
          return 0;
        }");

      ("loop with non-bool test expression",
       "int main(int argc, char* argv[]) {
          while (3) {}
          return 0;
        }");

      ("conditional with type error in then branch",
       "int main(int argc, char* argv[]) {
          if (1 == 1) {
            int x;
            int x;
          }
          return 0;
        }");

      ("conditional with type error in else branch",
       "int main(int argc, char* argv[]) {
          if (1 == 1) {} else {
            int x;
            int x;
          }
          return 0;
        }");

      ("loop with type error in body ",
       "int main(int argc, char* argv[]) {
          while (1 == 1) {
            int x;
            int x;
          }
          return 0;
        }");

      (* Expression related checks *)
      ("assignment to constant",
       "int main(int argc, char* argv[]) {
          5 = 42;
          return 0;
        }");

      ("assignment to array variable",
       "int main(int argc, char* argv[]) {
          int arr[5];
          arr = 42;
          return 0;
        }");

      ("assignment from array to array",
       "int main(int argc, char* argv[]) {
          int a1[5];
          int a2[5];
          a1[0] = 42;
          a2 = a1;
          return 0;
        }");

      ("mismatched pointer level assignment",
       "int main(int argc, char* argv[]) {
          int x;
          int* px;
          int** ppx;
          x = 0;
          px = &x;
          ppx = &px;
          *ppx = 42;
          return 0;
        }");

      ("variable use before declaration",
       "int main(int argc, char* argv[]) {
          int x;
          x = y;
          return 0;
        }");

      ("dereference a non-pointer identifier",
       "int main(int argc, char* argv[]) {
          int x;
          x = 0;
          *x = 5;
          return 0;
        }");

      ("logical or with non-bool lhs",
       "int main(int argc, char* argv[]) {
          5 || 1 == 1;
          return 0;
        }");

      ("logical or with non-bool rhs",
       "int main(int argc, char* argv[]) {
          1 == 1 || 5;
          return 0;
        }");

      ("logical and with non-bool lhs",
       "int main(int argc, char* argv[]) {
          5 && 1 == 1;
          return 0;
        }");

      ("logical and with non-bool rhs",
       "int main(int argc, char* argv[]) {
          1 == 1 && 5;
          return 0;
        }");

      ("logical not with non-bool expr",
       "int main(int argc, char* argv[]) {
          !5;
          return 0;
        }");

      ("equal with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p == 1;
          return 0;
        }");

      ("equal with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 == p;
          return 0;
        }");

      ("not equal with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p != 1;
          return 0;
        }");

      ("not equal with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 != p;
          return 0;
        }");

      ("less than with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int arr[5];
          arr < 5;
          return 0;
        }");

      ("less than with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          5 < p;
          return 0;
        }");

      ("less than or equal with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p <= 5;
          return 0;
        }");

      ("less than or equal with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          5 <= p;
          return 0;
        }");

      ("greater than with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int arr[5];
          arr > 5;
          return 0;
        }");

      ("greater than with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          5 > p;
          return 0;
        }");

      ("greater than or equal with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p >= 5;
          return 0;
        }");

      ("greater than or equal with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          5 >= p;
          return 0;
        }");

      ("addition with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p + 1;
          return 0;
        }");

      ("addition with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 + p;
          return 0;
        }");

      ("subtraction with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p - 1;
          return 0;
        }");

      ("subtraction with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 - p;
          return 0;
        }");

      ("multiplication with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p * 1;
          return 0;
        }");

      ("multiplication with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 * p;
          return 0;
        }");

      ("division with non-int-compatible lhs",
       "int main(int argc, char* argv[]) {
          int* p;
          p / 1;
          return 0;
        }");

      ("division with non-int-compatible rhs",
       "int main(int argc, char* argv[]) {
          int* p;
          1 / p;
          return 0;
        }");

      ("subject of cast doesn't typecheck",
       "int main(int argc, char* argv[]) {
          int arr[5];
          char* pc;
          pc = (char*) arr == 1;
          return 0;
        }");

      ("array ref with non-int-compatible index",
       "int main(int argc, char* argv[]) {
          int arr[50];
          arr[1 == 1];
          return 0;
        }");

      ("array reference on non-array identifier",
       "int main(int argc, char* argv[]) {
          int x;
          int y;
          x = y[5];
          return 0;
        }");

      ("function call with non-existent function",
       "int main(int argc, char* argv[]) {
          my_fn();
          return 0;
        }");

      ("calling a non-identifier as a function",
       "int main(int argc, char* argv[]) {
          5(45);
          return 0;
        }");

      ("function call with incompatible return type",
       "int* some_fn(void) {
          int* p;
          return p;
        }

        int main(int argc, char* argv[]) {
          int x;
          x = some_fn();
          return 0;
        }");

      ("function call with incompatible argument",
       "int some_fn(int* p) {
          return 0;
        }

        int main(int argc, char* argv[]) {
          int x;
          x = some_fn(x);
          return 0;
        }");

      ("negating a non-int-compatible type",
       "int main(int argc, char* argv[]) {
          int array[5];
          -array;
          return 0;
        }");

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
