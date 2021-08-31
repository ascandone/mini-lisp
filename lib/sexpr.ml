type t = Symbol of string | Number of float | List of t list [@@deriving eq]

let rec to_string expr =
  match expr with
  | Symbol str -> str
  | Number n -> string_of_float n
  | List exprs -> "(" ^ (exprs |> List.map to_string |> String.concat " ") ^ ")"
