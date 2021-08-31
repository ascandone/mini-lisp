open Lib

module Repl : sig
  val run : Eval.env -> unit
end = struct
  let evalute_results exprs env =
    match Eval.run_all env exprs with
    | Error err ->
        print_endline ("## Error: " ^ err);
        env
    | Ok (env, values) ->
        values
        |> List.map Eval.value_to_string
        |> String.concat "\n" |> print_endline;
        env

  let rec run env =
    print_string "> ";
    match read_line () with
    | ":{" -> multiline env
    | ":std" -> print env "(require \"stdlib.lisp\")"
    | ":refresh" -> run Eval.initial_env
    | ":exit" -> ()
    | ":env" ->
        print_endline (Eval.env_to_string env);
        run env
    | line -> print env line

  and print env line =
    match Parser.run line with
    | Ok exprs -> run (evalute_results exprs env)
    | Error e ->
        print_endline ("## Syntax Error: " ^ e);
        run env

  and multiline ?(acc = []) env =
    match read_line () with
    | ":}" -> acc |> List.rev |> String.concat "\n" |> print env
    | line -> multiline ~acc:(line :: acc) env
end

module Reader : sig
  val run : string -> unit
end = struct
  let run path =
    match Eval.run_file path with
    | Error e ->
        print_endline ("ERROR: " ^ e);
        ()
    | Ok _ -> ()
end

let get_path () = try Some Sys.argv.(1) with _ -> None

let () =
  match get_path () with
  | Some path -> Reader.run path
  | None -> Repl.run Eval.initial_env
