module StringMap = Map.Make (String)

type env_item =
  | Value of t
  | Macro of t list * t

and t =
  | Number of float
  | Symbol of string
  | Char of char
  | List of t list
  | Native of native_function
  | Closure of (env * t list * t)

and native_function = t list -> (t, string) result

and env = env_item StringMap.t

let rec pred_all mapper = function
  | [] -> Ok []
  | x :: xs ->
    (match mapper x with
    | Error e -> Error e
    | Ok x' -> pred_all mapper xs |> Result.map (fun xs' -> x' :: xs'))
;;

let char_list = function
  | List [] -> None
  | List exprs ->
    (match
       pred_all
         (function
           | Char s -> Ok s
           | e -> Error e)
         exprs
     with
    | Ok chars -> Some chars
    | Error _ -> None)
  | _ -> None
;;

let rec to_string expr =
  match char_list expr with
  | Some chars -> "\"" ^ Utils.string_of_chars chars ^ "\""
  | _ ->
    (match expr with
    | Symbol str -> str
    | Number n -> string_of_float n
    | Char ch -> "#\'" ^ Char.escaped ch ^ "'"
    | List [] -> "nil"
    | List exprs -> "(" ^ (exprs |> List.map to_string |> String.concat " ") ^ ")"
    | Native _ -> "[[Native function]]"
    | Closure _ -> "[[Lambda]]")
;;

let env_to_string (env : env) =
  let body =
    StringMap.bindings env
    |> List.map (fun (name, value) ->
           name
           ^ ": "
           ^
           match value with
           | Value value -> to_string value
           | Macro (_, _) -> "[[Macro]]")
    |> String.concat ", "
  in
  "{ " ^ body ^ " }"
;;

let rec equal v1 v2 =
  match v1, v2 with
  | Number x, Number y -> x = y
  | Symbol x, Symbol y -> x = y
  | Char x, Char y -> x = y
  | List x, List y -> List.equal equal x y
  | _ -> false
;;
