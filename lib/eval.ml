module StringMap = Map.Make (String)

type env_item = Value of value | Macro of (env * string list * value)

and value =
  | Number of float
  | Symbol of string
  | List of value list
  | Native of native_function
  | Lambda of (env * string list * value)

and native_function = value list -> (value, string) result

and env = env_item StringMap.t

let rec lift_sexpr = function
  | Sexpr.Number n -> Number n
  | Sexpr.Symbol s -> Symbol s
  | Sexpr.List l -> List (List.map lift_sexpr l)

let rec value_to_string expr =
  match expr with
  | Symbol str -> str
  | Number n -> string_of_float n
  | List [] -> "nil"
  | List exprs ->
      "(" ^ (exprs |> List.map value_to_string |> String.concat " ") ^ ")"
  | Native _ -> "[[Native function]]"
  | Lambda _ -> "[[Lambda]]"

let env_to_string (env : env) =
  let body =
    StringMap.bindings env
    |> List.map (fun (name, value) ->
           name ^ ": "
           ^
           match value with
           | Value value -> value_to_string value
           | Macro (_, _, _) -> "[[Macro]]")
    |> String.concat ", "
  in
  "{ " ^ body ^ " }"

let arity_error_msg label args =
  let n = Int.to_string (List.length args) in
  "Wrong number of args (" ^ n ^ ") passed to " ^ label

let rec pred_all mapper = function
  | [] -> Ok []
  | x :: xs -> (
      match mapper x with
      | Error e -> Error e
      | Ok x' -> pred_all mapper xs |> Result.map (fun xs' -> x' :: xs'))

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
    | [ _; _ ] -> Error "the second argument is expected to be a list"
    | args -> Error (arity_error_msg "cons" args)

  let isAtom = function
    | [ List [] ] | [ Symbol _ ] | [ Number _ ] -> Ok (vbool true)
    | [ _ ] -> Ok (vbool false)
    | args -> Error (arity_error_msg "atom?" args)

  let env =
    StringMap.empty
    |> bind_all
         [
           ("atom?", Native isAtom);
           ("+", Native plus);
           ("=", Native eq);
           ("head", Native head);
           ("tail", Native tail);
           ("cons", Native cons);
           ("println", Native println);
           ("nil", List []);
           ("true", Symbol "true");
           ("otherwise", Symbol "true");
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

  (* let map f (State runState) =
     State (fun env -> runState env |> Result.map (fun (env, x) -> (env, f x))) *)

  let bind (State runState) f =
    State
      (fun env ->
        Result.bind (runState env) (fun (env, x) ->
            let (State runState') = f x in
            runState' env))

  (* let ( let+ ) state f = map f state *)

  let ( let* ) = bind

  (* let ( >>= ) = bind *)

  let rec traverse f = function
    | [] -> return []
    | x :: xs ->
        let* x' = f x in
        let* xs' = traverse f xs in
        return (x' :: xs')
end

let rec zip_params params args =
  match (params, args) with
  | [], [] -> Some []
  | [], _ :: _ | _ :: _, [] -> None
  | param :: rest_params, arg :: rest_args ->
      zip_params rest_params rest_args
      |> Option.map (fun rest -> (param, arg) :: rest)

let rec quote_value =
  let open State in
  function
  | Symbol x -> return (Symbol x)
  | Number x -> return (Number x)
  | List (Symbol "unquote" :: args) -> (
      match args with
      | [ arg ] -> eval arg
      | _ -> fail @@ arity_error_msg "unquote" args)
  | List values ->
      let* list = traverse quote_value values in
      return (List list)
  | _ -> raise Exit

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
  | Lambda (scope_env, params_, body) :: args -> (
      match zip_params params_ args with
      | None -> fail @@ arity_error_msg "lambda" args
      | Some bindings ->
          let* env = get_env in
          let env' =
            StringMap.union (fun _ _ y -> Some y) scope_env env
            |> bind_all bindings
          in
          let* () = put_env env' in
          eval body)
  | Native nf :: args -> (
      match nf args with Ok r -> return r | Error e -> fail e)
  | v :: _ -> fail (value_to_string v ^ " is not a function")

and eval expr =
  let open State in
  match expr with
  | Native _ -> return expr
  | Lambda _ -> return expr
  | Number n -> return (Number n)
  | Symbol s -> (
      let* env = get_env in
      match StringMap.find_opt s env with
      | Some (Value value) -> return value
      | Some (Macro (_, _, _)) -> fail "Can't take value of a macro"
      | None -> fail ("unbound value: " ^ s))
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
          match
            pred_all (function Symbol s -> Ok s | e -> Error e) params
          with
          | Error _ -> fail "Parsing error in defmacro: params must be symbols"
          | Ok params_values ->
              let* env = get_env in
              let macro = Macro (env, params_values, body) in
              let* () = put_env (StringMap.add name macro env) in
              return (List []))
      | _ -> fail @@ arity_error_msg "defmacro" args)
  | List (Symbol "quote" :: args) -> (
      match args with [ arg ] -> quote_value arg | _ -> fail "Arity error")
  | List (Symbol "cond" :: args) -> eval_cond args
  | List (Symbol "lambda" :: args) -> (
      match args with
      | [ List params; body ] -> (
          match
            pred_all (function Symbol s -> Ok s | e -> Error e) params
          with
          | Error _ -> fail "Parsing error in lambda: params must be symbols"
          | Ok params_ ->
              let* env = get_env in
              return @@ Lambda (env, params_, body))
      | _ -> fail "Parsing error in lambda")
  | List (Symbol op :: args as forms) -> (
      let* env = get_env in

      match StringMap.find_opt op env with
      | Some (Macro (_, params, body)) -> (
          match zip_params params args with
          | None -> fail @@ arity_error_msg "defmacro" args
          | Some bindings ->
              let* env = get_env in
              let* () = put_env (env |> bind_all bindings) in
              let* value = eval body in
              eval value)
      | _ -> eval_application forms)
  | List forms -> eval_application forms

let run env expr = State.run (eval (lift_sexpr expr)) env

let run_all env exprs =
  State.run (State.traverse eval (List.map lift_sexpr exprs)) env
