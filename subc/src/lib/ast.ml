open Core

type subc_type =
  | Void
  | Int
  | Char
  | Bool
  | Pointer of subc_type
  | Array of subc_type
[@@deriving sexp]

type arg_list =
  | ArgVoid
  | ArgList of (subc_type * string) list
[@@deriving sexp]

type declaration =
  | VariableDeclaration of subc_type * string
  | ArrayDeclaration of subc_type * string * int
  | FunctionDeclaration of subc_type * string * arg_list
[@@deriving sexp]

type expression =
  | Assignment of expression * expression
  | LogicalOr of expression * expression
  | LogicalAnd of expression * expression
  | LogicalNot of expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | LessThan of expression * expression
  | LessThanEqual of expression * expression
  | GreaterThan of expression * expression
  | GreaterThanEqual of expression * expression
  | Add of expression * expression
  | Subtract of expression * expression
  | Multiply of expression * expression
  | Divide of expression * expression
  | Cast of subc_type * expression
  | ArrayRef of expression * expression
  | FunctionCall of expression * expression list
  | Id of string
  | IntConst of int
  | CharConst of char
  | AddressOf of expression
  | Dereference of expression
  | Negate of expression
[@@deriving sexp]

type statement =
  | Block of declaration list * statement list
  | Conditional of expression * statement * statement option
  | Expression of expression
  | Loop of expression * statement
  | Return of expression option
  | StmtVoid
[@@deriving sexp]

type subc_unit =
  | Declaration of declaration
  | FunctionDefinition of subc_type * string * arg_list * statement
[@@deriving sexp]

type ast =
  | Ast of subc_unit list
[@@deriving sexp]

(* Helper function for converting an AST into String form *)
let string_of_ast ast =
  match ast with
  | Ast prog_list -> List.to_string ~f:(fun elt -> Sexp.to_string (sexp_of_subc_unit elt)) prog_list
;;

let string_of_subc_type subc_type =
  Sexp.to_string (sexp_of_subc_type subc_type)
;;

let string_of_arg_list arg_list =
  Sexp.to_string (sexp_of_arg_list arg_list)
;;
