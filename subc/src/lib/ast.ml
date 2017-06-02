open Sexplib.Std

type subc_type =
  | Void
  | Int
  | Char
  | Pointer of subc_type
[@@deriving sexp]

module Arg = struct
  type t =
    {
      var_type: subc_type;
      id: string;
      array: bool;
    }
  [@@deriving sexp]
end;;

type arg_list =
  | Void
  | ArgList of Arg.t list
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

module FunctionDeclaration = struct
  type t =
    {
      ret_type: subc_type;
      id: string;
      arg_list: arg_list;
    }
  [@@deriving sexp]
end;;

type declaration =
  | Variable of Variable.t
  | Array of Array.t
  | FunctionDeclaration of FunctionDeclaration.t
[@@deriving sexp]

type subc_unit =
  | Declaration of declaration
  | FunctionDefinition
[@@deriving sexp]
