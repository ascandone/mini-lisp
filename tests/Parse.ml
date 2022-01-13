(* TEMP *)
[@@@warning "-32"]

open Lib

let check_parse ~msg ~source ~expected =
  Alcotest.(check' (result (list Testable.value) string))
    ~msg
    ~expected:(Ok [ expected ])
    ~actual:(Parser.run source)
;;

(* Number *)

let test_int () =
  check_parse ~msg:"\"0\" should be parsed a literal" ~source:"0" ~expected:(Number 0.)
;;

let test_int_2 () =
  check_parse ~msg:"\"42\" should be parsed a literal" ~source:"42" ~expected:(Number 42.)
;;

let test_float () =
  check_parse
    ~msg:"\"1.234\" should be parsed a literal"
    ~source:"1.234"
    ~expected:(Number 1.234)
;;

(* Strings *)

let test_string_literal () =
  check_parse
    ~msg:"the string syntax sugar should be parsed as a list of chars"
    ~source:{| "ab" |}
    ~expected:(List [ Symbol "quote"; List [ Char 'a'; Char 'b' ] ])
;;

let () =
  let open Alcotest in
  run
    "Parsing"
    [ ( "number"
      , [ test_case "int" `Quick test_int
        ; test_case "int" `Quick test_int_2
          (* test_case "float" `Quick test_float; *)
        ] )
    ; "string", [ test_case "string syntax" `Quick test_string_literal ]
    ]
;;
