module type Ctx = sig
  type t
end

module Make (Ctx : Ctx) = struct
  type 'a t = State of (Ctx.t -> (Ctx.t * 'a, string) result)

  let run (State run) = run

  let get_env = State (fun ctx -> Ok (ctx, ctx))

  let put_env ctx = State (fun _ -> Ok (ctx, ()))

  let return x = State (fun ctx -> Ok (ctx, x))

  let fail reason = State (fun _ -> Error reason)

  let map f (State runState) =
    let open Utils.LetSyntax.Result in
    State
      (fun env ->
        let+ env, x = runState env in
        (env, f x))

  let bind (State runState) f =
    let open Utils.LetSyntax.Result in
    State
      (fun env ->
        let* env, x = runState env in
        let (State runState') = f x in
        runState' env)

  let ( let+ ) state f = map f state

  let ( let* ) = bind

  (* let ( >>= ) = bind *)

  let rec traverse f = function
    | [] -> return []
    | x :: xs ->
        let* x' = f x in
        let+ xs' = traverse f xs in
        x' :: xs'
end
