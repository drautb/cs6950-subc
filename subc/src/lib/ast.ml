open Sexplib.Std

type subc_type =
  | Int
  | Char
  | Pointer of subc_type
[@@deriving sexp]

module Variable = struct
  type t =
    {
      var_type: subc_type;
      id: string;
    }
  [@@deriving sexp]
end;;

module Array = struct
  type t =
    {
      var_type: subc_type;
      id: string;
      size: int;
    }
  [@@deriving sexp]
end;;

type declaration =
  | Variable of Variable.t
  | Array of Array.t
  | Function
[@@deriving sexp]

type subc_unit =
  | Declaration of declaration
  | FunctionDefinition
[@@deriving sexp]
