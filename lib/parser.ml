open Angstrom
open Sexpr

let quote_symbol = "`"

let unquote_symbol = ","

let whitespace =
  many @@ satisfy
  @@ fun ch -> match ch with ' ' | '\t' | '\n' -> true | _ -> false

let is_digit ch = match ch with '0' .. '9' -> true | _ -> false

let is_char ch = match ch with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let pred_or f g x = f x || g x

let is_identifier_char ch =
  match ch with
  | '*' | '/' | '+' | '!' | '-' | '_' | '?' | '<' | '>' | '=' -> true
  | _ -> false

let number_literal =
  (* TODO float *)
  let* integer = many1 @@ satisfy @@ is_digit in
  try return @@ Number (float_of_string @@ Utils.string_of_chars integer)
  with _ -> fail "invalid number"

let symbol =
  let is_leading = is_identifier_char |> pred_or is_char in
  let* leading = satisfy is_leading in
  let* rest = many @@ satisfy @@ (is_leading |> pred_or is_digit) in
  return @@ Symbol (Utils.string_of_chars (leading :: rest))

let list_ expr =
  let* _ = string "(" in
  let* _ = whitespace in
  let* exprs = sep_by whitespace expr in
  let* _ = whitespace in
  let* _ = string ")" in
  return @@ List exprs

let quoted expr =
  let* _ = string quote_symbol in
  let* _ = whitespace in
  let* e = expr in
  return @@ List [ Symbol "quote"; e ]

let unquoted expr =
  let* _ = string unquote_symbol in
  let* _ = whitespace in
  let* e = expr in
  return @@ List [ Symbol "unquote"; e ]

let char_literal =
  let* _ = char '\'' in
  let* c = any_char in
  let* _ = char '\'' in
  return (Char c)

let string_literal =
  let* _ = char '\"' in
  let* chars = take_while (( != ) '\"') in
  let* _ = char '\"' in
  let lst = chars |> Utils.chars_of_string |> List.map (fun ch -> Char ch) in
  return @@ List [ Symbol "quote"; List lst ]

(* (List [ Char 'a'; Char 'b' ]) *)

let expr =
  fix @@ fun expr ->
  choice ~failure_msg:"Invalid expression"
    [
      symbol;
      number_literal;
      list_ expr;
      quoted expr;
      unquoted expr;
      char_literal;
      string_literal;
    ]

let parser = whitespace *> sep_by whitespace expr <* whitespace

let run = parse_string ~consume:All parser
