module type Ctx = sig
  type t
end

module Make (Ctx : Ctx) = struct
  type 'a t = State of (Ctx.t -> (Ctx.t * 'a, string) result)

  let run (State run) = run

  let get_ctx = State (fun ctx -> Ok (ctx, ctx))

  let put_ctx ctx = State (fun _ -> Ok (ctx, ()))

  let return x = State (fun ctx -> Ok (ctx, x))

  let fail reason = State (fun _ -> Error reason)

  let map f (State runState) =
    let open Utils.LetSyntax.Result in
    State
      (fun ctx ->
        let+ ctx, x = runState ctx in
        (ctx, f x))

  let bind (State runState) f =
    let open Utils.LetSyntax.Result in
    State
      (fun ctx ->
        let* ctx, x = runState ctx in
        let (State runState') = f x in
        runState' ctx)

  let ( let+ ) state f = map f state

  let ( let* ) = bind

  let update_ctx f =
    let* ctx = get_ctx in
    put_ctx (f ctx)

  (* let ( >>= ) = bind *)

  let rec traverse f = function
    | [] -> return []
    | x :: xs ->
        let* x' = f x in
        let+ xs' = traverse f xs in
        x' :: xs'
end
