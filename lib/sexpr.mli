(* include module type of Sexpr *)

type t = Symbol of string | Number of float | Char of char | List of t list
