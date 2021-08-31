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
        ()

  and multiline ?(acc = []) env =
    match read_line () with
    | ":}" -> acc |> List.rev |> String.concat "\n" |> print env
    | line -> multiline ~acc:(line :: acc) env
end

module Reader : sig
  val run : string -> unit
end = struct
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let run path =
    match Parser.run (read_whole_file path) with
    | Error e -> print_endline ("Parsing error: " ^ e)
    | Ok exprs -> (
        match Eval.run_all Eval.initial_env exprs with
        | Error e -> print_endline ("Error: " ^ e)
        | Ok _ -> ())
end

let () = try Reader.run Sys.argv.(1) with _ -> Repl.run Eval.initial_env
