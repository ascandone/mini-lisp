open Value
open Utils

module State = State.Make (struct
  type t = env
end)

let shadow_env env env' = StringMap.union (fun _ _ y -> Some y) env env'

let bind_all bindings env =
  bindings
  |> List.fold_left (fun e (param, value) -> StringMap.add param (Value value) e) env
;;

let truthy = function
  | Symbol "true" -> true
  | _ -> false
;;

let initial_env = bind_all Prelude.env StringMap.empty

let rec zip_optional_params params args =
  let open Utils.LetSyntax.Result in
  match params, args with
  | [], [] -> Ok []
  | [], _ :: _ -> Error `Arity
  | Symbol name :: rest_params, [] ->
    let+ rest = zip_optional_params rest_params [] in
    (name, List []) :: rest
  | Symbol name :: rest_params, arg :: rest_args ->
    let+ rest = zip_optional_params rest_params rest_args in
    (name, arg) :: rest
  | _ -> Error `OptionalParam
;;

let rec kw_args_to_map =
  let open Utils.LetSyntax.Result in
  function
  | [] -> Ok StringMap.empty
  | Symbol kw :: value :: args ->
    let+ rest = kw_args_to_map args in
    StringMap.add kw value rest
  | _ -> Error `Kw
;;

let rec zip_kw_args params args_map =
  let open Utils.LetSyntax.Result in
  match params with
  | [] -> Ok []
  | Symbol param :: params' ->
    let value =
      match StringMap.find_opt param args_map with
      | None -> List []
      | Some value -> value
    in
    let+ rest = zip_kw_args params' args_map in
    (param, value) :: rest
  | _ -> Error `Kw
;;

let rec zip_params params args =
  let open Utils.LetSyntax.Result in
  match params, args with
  | Symbol "&rest" :: Symbol name :: _, args -> Ok [ name, List args ]
  | Symbol "&opt" :: params, args -> zip_optional_params params args
  | Symbol "&key" :: params, args ->
    let* args_map = kw_args_to_map args in
    zip_kw_args params args_map
  | List params :: rest_params, List args :: rest_args ->
    let* p = zip_params params args in
    let+ p' = zip_params rest_params rest_args in
    List.append p p'
  | List _ :: _, _ -> Error `Destructuring
  | [], [] -> Ok []
  | [], _ :: _ | _ :: _, [] -> Error `Arity
  | Symbol name :: rest_params, arg :: rest_args ->
    let+ rest = zip_params rest_params rest_args in
    (name, arg) :: rest
  | _ -> Error `Invalid_arg
;;

let args_err_of_string ~ctx ~args = function
  | `Arity -> arity_error_msg ctx args
  | `Destructuring -> "destructuring error"
  | `OptionalParam -> "optional parameter syntax error"
  | `Invalid_arg -> "optional parameter syntax error"
  | `Kw -> "Keword arguments error"
;;

let parse_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  Parser.run s
;;

let with_env f =
  let open State in
  let* backup_env = get_ctx in
  let* res = f backup_env in
  let+ () = put_ctx backup_env in
  res
;;

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
  | Lambda (scope_env, params, body) :: args ->
    (match zip_params params args with
    | Error err -> fail @@ args_err_of_string ~ctx:"lambda" ~args err
    | Ok bindings ->
      with_env
      @@ fun env ->
      let env' = shadow_env env scope_env |> bind_all bindings in
      let* () = put_ctx env' in
      eval body)
  | Native nf :: args ->
    (match nf args with
    | Ok r -> return r
    | Error e -> fail e)
  | v :: _ -> fail (Value.to_string v ^ " is not a function")

and eval value =
  let open State in
  match value with
  | Native _ | Lambda _ | Char _ | Number _ -> return value
  | Symbol s ->
    let* env = get_ctx in
    (match StringMap.find_opt s env with
    | Some (Value value) -> return value
    | Some (Macro (_, _)) -> fail "Can't take value of a macro"
    | None -> fail ("unbound value: " ^ s))
  | List values -> eval_form values

and eval_form forms =
  let open State in
  match forms with
  | Symbol "require" :: path_symbol :: _ ->
    (* TODO import selection  *)
    let* path = eval path_symbol in
    (match char_list path with
    | None ->
      fail @@ "require path must be a string, got <" ^ Value.to_string path ^ "> instead"
    | Some chars ->
      let* backup_env = get_ctx in
      let* () = put_ctx initial_env in
      let* _ = eval_file (Utils.string_of_chars chars) in
      let* file_env = get_ctx in
      let+ () = put_ctx (shadow_env backup_env file_env) in
      List [])
  | Symbol "def" :: args ->
    (match args with
    | [ Symbol name; form ] ->
      let* value = eval form in
      let* env = get_ctx in
      let+ () = put_ctx (StringMap.add name (Value value) env) in
      List []
    | _ -> fail @@ arity_error_msg "def" args)
  | Symbol "defmacro" :: args ->
    (match args with
    | [ Symbol name; List params; body ] ->
      let* env = get_ctx in
      let macro = Macro (params, body) in
      let* () = put_ctx (StringMap.add name macro env) in
      return (List [])
    | _ -> fail @@ arity_error_msg "defmacro" args)
  | Symbol "do" :: args ->
    let+ values = traverse eval args in
    (match List.rev values with
    | [] -> List []
    | last :: _ -> last)
  | Symbol "quote" :: args ->
    (match args with
    | [ arg ] -> quote_value arg
    | _ -> fail "Arity error")
  | Symbol "cond" :: args -> eval_cond args
  | Symbol "lambda" :: args ->
    (match args with
    | [ List values; body ] ->
      let+ env = get_ctx in
      Lambda (env, values, body)
    | _ -> fail "Parsing error in lambda")
  | Symbol op :: args as forms ->
    let* env = get_ctx in
    (match StringMap.find_opt op env with
    | Some (Macro (params, body)) ->
      (match zip_params params args with
      | Error err -> fail @@ args_err_of_string ~ctx:"macro" ~args err
      | Ok bindings ->
        let* env = get_ctx in
        let* () = put_ctx (env |> bind_all bindings) in
        let* value = eval body in
        let* () = put_ctx env in
        eval value)
    | _ -> eval_application forms)
  | _ -> eval_application forms

and eval_file path =
  let open State in
  match parse_file path with
  | Error e -> fail ("Parsing error: " ^ e)
  | Ok exprs ->
    let+ _ = traverse eval exprs in
    List []
;;

let run env value = State.run (eval value) env

let run_all ?(debug_read = false) env values =
  if debug_read
  then (
    print_string "=> ";
    values |> List.map Value.to_string |> String.concat " " |> print_endline)
  else ();
  State.run (State.traverse eval values) env
;;

let run_file path = State.run (eval_file path) initial_env
