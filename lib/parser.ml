open Angstrom
open Value
(* consts *)

let quote_symbol = "'"

let backquote_symbol = "`"

let unquote_symbol = "~"

let unquote_splicing_symbol = "~@"

let whitespace =
  many @@ satisfy @@ function ' ' | '\t' | '\n' | ',' -> true | _ -> false

let is_digit ch = match ch with '0' .. '9' -> true | _ -> false

let is_char ch = match ch with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let pred_or f g x = f x || g x

let is_identifier_char ch =
  match ch with
  | '*' | '/' | '+' | '!' | '-' | '_' | '?' | '<' | '>' | '=' | '&' -> true
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

let list_ expr ~backquoted =
  let* _ = string "(" in
  let* _ = whitespace in
  let* exprs =
    sep_by whitespace
      (if backquoted then
       let* backquote_type =
         choice
           [
             string unquote_splicing_symbol *> whitespace *> return `Splice;
             string unquote_symbol *> whitespace *> return `None;
             return `Backquote;
           ]
       in
       let* e = expr ~backquoted:(backquote_type == `Backquote) in
       return
         (match backquote_type with
         | `Splice -> e
         | _ -> List [ Symbol "list"; e ])
      else expr ~backquoted)
  in
  let* _ = whitespace in
  let* _ = string ")" in
  return @@ if backquoted then List (Symbol "concat" :: exprs) else List exprs

let quote e = List [ Symbol "quote"; e ]

let quoted expr ~backquoted =
  let* _ = string quote_symbol in
  let* _ = whitespace in
  let* e = expr ~backquoted in
  return @@ quote e

let char_literal =
  let* _ = string "#\'" in
  let* c = any_char in
  let* _ = char '\'' in
  return (Char c)

let string_literal =
  let* _ = char '\"' in
  let* chars = take_while (( != ) '\"') in
  let* _ = char '\"' in
  let lst = chars |> Utils.chars_of_string |> List.map (fun ch -> Char ch) in
  return @@ quote (List lst)

let backquote_atom ~backquoted p =
  let* value = p in
  return @@ if backquoted then quote value else value

let rec expr_p ~backquoted =
  choice
    [
      (* Atoms *)
      number_literal;
      char_literal;
      backquote_atom ~backquoted symbol;
      (* Forms *)
      list_ expr_p ~backquoted;
      string_literal;
      quoted expr_p ~backquoted;
      (* Recursion *)
      (let* _ = string backquote_symbol in
       let* backquoted =
         option true (string unquote_symbol *> whitespace *> return false)
       in
       let+ value = expr_p ~backquoted in
       if backquoted then match value with List _ -> value | _ -> quote value
       else value);
    ]

let parser =
  whitespace *> sep_by whitespace (expr_p ~backquoted:false) <* whitespace

let run = parse_string ~consume:All parser
