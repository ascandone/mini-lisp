open Value
open Utils

let nil = List []
let vbool b = if b then Symbol "true" else nil

let plus values =
  match
    pred_all
      (function
        | Number s -> Ok s
        | e -> Error e)
      values
  with
  | Ok nums -> Ok (Number (List.fold_left ( +. ) 0. nums))
  | Error _ -> Error "Sum expects numbers as arguments"
;;

let println values =
  values |> List.map Value.to_string |> String.concat " " |> print_endline;
  Ok (List [])
;;

let eq = function
  | [ List []; List [] ] -> Ok (vbool true)
  | [ Symbol x; Symbol y ] when x = y -> Ok (vbool true)
  | [ Number x; Number y ] when x = y -> Ok (vbool true)
  | [ Char x; Char y ] when x = y -> Ok (vbool true)
  | [ _; _ ] -> Ok (vbool false)
  | args -> Error (arity_error_msg "=" args)
;;

let head = function
  | [ List (hd :: _) ] -> Ok hd
  | [ _ ] -> Ok nil
  | args -> Error (arity_error_msg "head" args)
;;

let tail = function
  | [ List (_ :: tl) ] -> Ok (List tl)
  | [ _ ] -> Ok nil
  | args -> Error (arity_error_msg "tail" args)
;;

let cons = function
  | [ x; List xs ] -> Ok (List (x :: xs))
  | [ _; _ ] -> Error "Cons: the second argument is expected to be a list"
  | args -> Error (arity_error_msg "cons" args)
;;

let is_atom = function
  | [ List [] ] | [ Symbol _ ] | [ Number _ ] | [ Char _ ] -> Ok (vbool true)
  | [ _ ] -> Ok (vbool false)
  | args -> Error (arity_error_msg "atom?" args)
;;

let make_string str = List (List.map (fun ch -> Char ch) @@ Utils.chars_of_string str)

let tag = function
  | List _ -> "list"
  | Char _ -> "char"
  | Number _ -> "number"
  | Symbol _ -> "symbol"
  | Closure _ -> "closure"
  | Native _ -> "function"
;;

let type_of = function
  | [ arg ] -> Ok (make_string (tag arg))
  | args -> Error (arity_error_msg "type-of" args)
;;

let env =
  [ "atom?", Native is_atom
  ; "+", Native plus
  ; "=", Native eq
  ; "head", Native head
  ; "tail", Native tail
  ; "cons", Native cons
  ; "println", Native println
  ; "type-of", Native type_of
  ]
;;
