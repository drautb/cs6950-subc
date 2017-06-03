open Sexplib.Std

type subc_type =
  | Void
  | Int
  | Char
  | Pointer of subc_type
[@@deriving sexp]

type arg_list =
  | ArgVoid
  | ArgList of (subc_type * string * bool) list
[@@deriving sexp]

type declaration =
  | Variable of (subc_type * string)
  | Array of (subc_type * string * int)
  | FunctionDeclaration of (subc_type * string * arg_list)
[@@deriving sexp]

type statement =
  | Statement
[@@deriving sexp]

type block =
   | Block of (declaration list * statement list)
[@@deriving sexp]

type subc_unit =
  | Declaration of declaration
  | FunctionDefinition of (subc_type * string * arg_list * block)
[@@deriving sexp]
