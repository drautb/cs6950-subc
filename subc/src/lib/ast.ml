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
  | Block of (declaration list * statement list)
  | Conditional of (expression * statement * statement option)
  | Expression of expression
  | Loop of (expression * statement)
  | Return of expression option
  | StmtVoid
[@@deriving sexp]

type subc_unit =
  | Declaration of declaration
  | FunctionDefinition of (subc_type * string * arg_list * statement)
[@@deriving sexp]
