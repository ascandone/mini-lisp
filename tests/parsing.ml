open Lib

let value = Alcotest.testable (Fmt.of_to_string Value.to_string) Value.equal

let check_parse msg s1 s2 =
  Alcotest.(check (result (list value) string))
    msg (Parser.run s1) (Parser.run s2)

let () =
  let open Alcotest in
  run "Utils"
    [
      ( "parsing",
        [
          test_case "Lower case" `Quick (fun () ->
              check_parse "string syntax sugar"
                {| '(#'h' #'e' #'l' #'l' #'o') |} {| "hello" |});
        ] );
    ]
