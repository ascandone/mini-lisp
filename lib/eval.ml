module StringMap = Map.Make (String)

type param = Param of string | Destruct of param list

type env_item = Value of value | Macro of param list * value

and value =
  | Number of float
  | Symbol of string
  | Char of char
  | List of value list
  | Native of native_function
  | Lambda of (env * param list * value)

and native_function = value list -> (value, string) result

and env = env_item StringMap.t

let shadow_env env env' = StringMap.union (fun _ _ y -> Some y) env env'

let rec lift_sexpr = function
  | Sexpr.Number n -> Number n
  | Sexpr.Symbol s -> Symbol s
  | Sexpr.Char ch -> Char ch
  | Sexpr.List l -> List (List.map lift_sexpr l)

let rec pred_all mapper = function
  | [] -> Ok []
  | x :: xs -> (
      match mapper x with
      | Error e -> Error e
      | Ok x' -> pred_all mapper xs |> Result.map (fun xs' -> x' :: xs'))

let char_list = function
  | List [] -> None
  | List exprs -> (
      match pred_all (function Char s -> Ok s | e -> Error e) exprs with
      | Ok chars -> Some chars
      | Error _ -> None)
  | _ -> None

let rec value_to_string expr =
  match char_list expr with
  | Some chars -> "\"" ^ Utils.string_of_chars chars ^ "\""
  | _ -> (
      match expr with
      | Symbol str -> str
      | Number n -> string_of_float n
      | Char ch -> "#\'" ^ Char.escaped ch ^ "'"
      | List [] -> "nil"
      | List exprs ->
          "(" ^ (exprs |> List.map value_to_string |> String.concat " ") ^ ")"
      | Native _ -> "[[Native function]]"
      | Lambda _ -> "[[Lambda]]")

let env_to_string (env : env) =
  let body =
    StringMap.bindings env
    |> List.map (fun (name, value) ->
           name ^ ": "
           ^
           match value with
           | Value value -> value_to_string value
           | Macro (_, _) -> "[[Macro]]")
    |> String.concat ", "
  in
  "{ " ^ body ^ " }"

let arity_error_msg label args =
  let n = Int.to_string (List.length args) in
  "Wrong number of args (" ^ n ^ ") passed to " ^ label

let bind_all bindings env =
  bindings
  |> List.fold_left
       (fun e (param, value) -> StringMap.add param (Value value) e)
       env

let truthy = function Symbol "true" -> true | _ -> false

module Prelude : sig
  val env : env
end = struct
  let nil = List []

  let vbool b = if b then Symbol "true" else nil

  let plus values =
    match pred_all (function Number s -> Ok s | e -> Error e) values with
    | Ok nums -> Ok (Number (List.fold_left ( +. ) 0. nums))
    | Error _ -> Error "Sum expects numbers as arguments"

  let println values =
    values |> List.map value_to_string |> String.concat " " |> print_endline;
    Ok (List [])

  let eq = function
    | [ List []; List [] ] -> Ok (vbool true)
    | [ Symbol x; Symbol y ] when x = y -> Ok (vbool true)
    | [ Number x; Number y ] when x = y -> Ok (vbool true)
    | [ Char x; Char y ] when x = y -> Ok (vbool true)
    | [ _; _ ] -> Ok (vbool false)
    | args -> Error (arity_error_msg "=" args)

  let head = function
    | [ List (hd :: _) ] -> Ok hd
    | [ _ ] -> Ok nil
    | args -> Error (arity_error_msg "head" args)

  let tail = function
    | [ List (_ :: tl) ] -> Ok (List tl)
    | [ _ ] -> Ok nil
    | args -> Error (arity_error_msg "tail" args)

  let cons = function
    | [ x; List xs ] -> Ok (List (x :: xs))
    | [ _; _ ] -> Error "Cons: the second argument is expected to be a list"
    | args -> Error (arity_error_msg "cons" args)

  let is_atom = function
    | [ List [] ] | [ Symbol _ ] | [ Number _ ] | [ Char _ ] -> Ok (vbool true)
    | [ _ ] -> Ok (vbool false)
    | args -> Error (arity_error_msg "atom?" args)

  let make_string str =
    List (List.map (fun ch -> Char ch) @@ Utils.chars_of_string str)

  let tag = function
    | List _ -> "list"
    | Char _ -> "char"
    | Number _ -> "number"
    | Symbol _ -> "symbol"
    | Lambda _ -> "closure"
    | Native _ -> "function"

  let type_of = function
    | [ arg ] -> Ok (make_string (tag arg))
    | args -> Error (arity_error_msg "type-of" args)

  let env =
    StringMap.empty
    |> bind_all
         [
           ("atom?", Native is_atom);
           ("+", Native plus);
           ("=", Native eq);
           ("head", Native head);
           ("tail", Native tail);
           ("cons", Native cons);
           ("println", Native println);
           ("type-of", Native type_of);
         ]
end

let initial_env = Prelude.env

module State = struct
  type 'a t = State of (env -> (env * 'a, string) result)

  let run (State run) = run

  let get_env = State (fun env -> Ok (env, env))

  let put_env env = State (fun _ -> Ok (env, ()))

  let return x = State (fun env -> Ok (env, x))

  let fail reason = State (fun _ -> Error reason)

  let map f (State runState) =
    State (fun env -> runState env |> Result.map (fun (env, x) -> (env, f x)))

  let bind (State runState) f =
    State
      (fun env ->
        Result.bind (runState env) (fun (env, x) ->
            let (State runState') = f x in
            runState' env))

  let ( let+ ) state f = map f state

  let ( let* ) = bind

  (* let ( >>= ) = bind *)

  let rec traverse f = function
    | [] -> return []
    | x :: xs ->
        let* x' = f x in
        let* xs' = traverse f xs in
        return (x' :: xs')
end

let rec extract_params = function
  | [] -> Ok []
  | Symbol name :: xs ->
      extract_params xs |> Result.map (fun xs' -> Param name :: xs')
  | List params :: xs ->
      Result.bind (extract_params params) (fun ps ->
          extract_params xs |> Result.map (fun xs' -> Destruct ps :: xs'))
  | _ -> Error "Invalid param"

(*

  (lambda (x y (a &optional b) z)
    nil)

  (let ((x &opt y) '(1 2))
    `(~x ~y)) => nil

  
  (let (
      (x &opt y) '(1)
    )
    `(~x ~y)) => nil
*)

let rec zip_optional_params params args =
  let open Utils.LetSyntax.Result in
  match (params, args) with
  | [], [] -> Ok []
  | [], _ :: _ -> Error `Arity
  | Param name :: rest_params, [] ->
      let+ rest = zip_optional_params rest_params [] in
      (name, List []) :: rest
  | Param name :: rest_params, arg :: rest_args ->
      let+ rest = zip_optional_params rest_params rest_args in
      (name, arg) :: rest
  | _ -> Error `OptionalParam

let rec zip_params params args =
  let open Utils.LetSyntax.Result in
  match (params, args) with
  | Param "&rest" :: Param name :: _, args -> Ok [ (name, List args) ]
  | Param "&opt" :: params, args -> zip_optional_params params args
  | Destruct params :: rest_params, List args :: rest_args ->
      let* p = zip_params params args in
      let+ p' = zip_params rest_params rest_args in
      List.append p p'
  | Destruct _ :: _, _ -> Error `Destructuring
  | [], [] -> Ok []
  | [], _ :: _ | _ :: _, [] -> Error `Arity
  | Param name :: rest_params, arg :: rest_args ->
      let+ rest = zip_params rest_params rest_args in
      (name, arg) :: rest

let parse_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  Parser.run s

let with_env f =
  let open State in
  let* backup_env = get_env in
  let* res = f backup_env in
  let* () = put_env backup_env in
  return res

let rec quote_value value =
  let open State in
  match value with
  | List values ->
      let+ list = traverse quote_value values in
      List list
  | _ -> return value

and eval_cond =
  let open State in
  function
  | [] -> return (List [])
  | b :: y :: rest ->
      let* b_value = eval b in
      if truthy b_value then eval y else eval_cond rest
  | _ -> fail "Uneven clauses in cond"

and eval_application forms =
  let open State in
  let* values = traverse eval forms in
  match values with
  | [] -> return (List [])
  | Lambda (scope_env, params, body) :: args -> (
      match zip_params params args with
      | Error `Arity -> fail @@ arity_error_msg "lambda" args
      | Error `Destructuring -> fail "destructuring error"
      | Error `OptionalParam -> fail "optional parameter syntax error"
      | Ok bindings ->
          with_env @@ fun env ->
          let env' = shadow_env env scope_env |> bind_all bindings in
          let* () = put_env env' in
          eval body)
  | Native nf :: args -> (
      match nf args with Ok r -> return r | Error e -> fail e)
  | v :: _ -> fail (value_to_string v ^ " is not a function")

and eval expr =
  let open State in
  match expr with
  | Native _ | Lambda _ | Char _ | Number _ -> return expr
  | Symbol s -> (
      let* env = get_env in
      match StringMap.find_opt s env with
      | Some (Value value) -> return value
      | Some (Macro (_, _)) -> fail "Can't take value of a macro"
      | None -> fail ("unbound value: " ^ s))
  | List (Symbol "require" :: path_symbol :: _) -> (
      (* TODO import selection  *)
      let* path = eval path_symbol in
      match char_list path with
      | None ->
          fail @@ "require path must be a string, got <" ^ value_to_string path
          ^ "> instead"
      | Some chars ->
          let* backup_env = get_env in
          let* () = put_env initial_env in
          let* _ = eval_file (Utils.string_of_chars chars) in
          let* file_env = get_env in
          let* () = put_env (shadow_env backup_env file_env) in
          return (List []))
  | List (Symbol "def" :: args) -> (
      match args with
      | [ Symbol name; form ] ->
          let* value = eval form in
          let* env = get_env in
          let* () = put_env (StringMap.add name (Value value) env) in
          return (List [])
      | _ -> fail @@ arity_error_msg "def" args)
  | List (Symbol "defmacro" :: args) -> (
      match args with
      | [ Symbol name; List params; body ] -> (
          match extract_params params with
          | Error _ -> fail "Parsing error in defmacro: params must be symbols"
          | Ok params_values ->
              let* env = get_env in
              let macro = Macro (params_values, body) in
              let* () = put_env (StringMap.add name macro env) in
              return (List []))
      | _ -> fail @@ arity_error_msg "defmacro" args)
  | List (Symbol "do" :: args) -> (
      let+ values = traverse eval args in
      match List.rev values with [] -> List [] | last :: _ -> last)
  | List (Symbol "quote" :: args) -> (
      match args with [ arg ] -> quote_value arg | _ -> fail "Arity error")
  | List (Symbol "cond" :: args) -> eval_cond args
  | List (Symbol "lambda" :: args) -> (
      match args with
      | [ List values; body ] -> (
          match extract_params values with
          | Error _ -> fail "Parsing error in lambda: params must be symbols"
          | Ok params' ->
              let+ env = get_env in
              Lambda (env, params', body))
      | _ -> fail "Parsing error in lambda")
  | List (Symbol op :: args as forms) -> (
      let* env = get_env in
      match StringMap.find_opt op env with
      | Some (Macro (params, body)) -> (
          match zip_params params args with
          | Error `Arity -> fail @@ arity_error_msg op args
          | Error `Destructuring -> fail "destructuring error in macro"
          | Error `OptionalParam ->
              fail "optional parameter syntax error in macro"
          | Ok bindings ->
              let* env = get_env in
              let* () = put_env (env |> bind_all bindings) in
              let* value = eval body in
              let* () = put_env env in
              eval value)
      | _ -> eval_application forms)
  | List forms -> eval_application forms

and eval_file path =
  let open State in
  match parse_file path with
  | Error e -> fail ("Parsing error: " ^ e)
  | Ok exprs ->
      let+ _ = traverse eval (List.map lift_sexpr exprs) in
      List []

let run env expr = State.run (eval (lift_sexpr expr)) env

let run_all ?(debug_read = false) env exprs =
  let values = exprs |> List.map lift_sexpr in
  if debug_read then (
    print_string "=> ";
    values |> List.map value_to_string |> String.concat " " |> print_endline)
  else ();
  State.run (State.traverse eval values) env

let run_file path = State.run (eval_file path) initial_env
