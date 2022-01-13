open Lib

let last lst =
  match lst with
  | [] -> failwith "Empty list"
  | hd :: tl -> List.fold_left (fun _ x -> x) hd tl
;;

let check_eval ~msg ~expected ~actual =
  let actual =
    Result.bind (Parser.run actual) (fun value -> Eval.run_all Eval.initial_env value)
    |> Result.get_ok
    |> fun (_env, value) -> last value
  in
  let expected = Parser.run expected |> Result.get_ok |> last in
  Alcotest.(check' Testable.value) ~msg ~expected ~actual
;;

let () =
  let open Alcotest in
  run
    "Eval"
    [ ( "constants"
      , [ test_case "num" `Quick (fun () ->
              check_eval ~msg:"number" ~actual:{| 0 |} ~expected:{| 0 |})
        ; test_case "nil" `Quick (fun () ->
              check_eval ~msg:"nil" ~actual:{| () |} ~expected:{| () |})
        ] )
    ; ( "quoted"
      , [ test_case "nil" `Quick (fun () ->
              check_eval ~msg:"nil" ~actual:{| '() |} ~expected:{| () |})
        ; test_case "nonempty list" `Quick (fun () ->
              check_eval ~msg:"nil" ~actual:{| '(x y z) |} ~expected:{| (x y z) |})
        ] )
    ; ( "special forms"
      , [ test_case "def" `Quick (fun () ->
              check_eval ~msg:"def" ~actual:{| (def x 42) x |} ~expected:{| 42 |})
        ; test_case "lambda" `Quick (fun () ->
              check_eval
                ~msg:"lambda (simple)"
                ~actual:
                  {|
                    (def f (lambda (x) (+ 1 x)))
                    (f 10)
                  |}
                ~expected:{| 11 |})
        ; test_case "lambda" `Quick (fun () ->
              check_eval
                ~msg:"lambda (curried)"
                ~actual:
                  {|
                    (def f (lambda (x) (lambda (y) x)))
                    ((f #'x') #'y')
                  |}
                ~expected:{| #'x' |})
        ; test_case "lambda" `Quick (fun () ->
              check_eval
                ~msg:"lambda (curried)"
                ~actual:
                  {|
                    (def f (lambda (x) (lambda (y) y)))
                    ((f #'x') #'y')
                  |}
                ~expected:{| #'y' |})
        ; test_case "lambda" `Quick (fun () ->
              check_eval
                ~msg:"lambda (curried with shadowing)"
                ~actual:
                  {|
                    (def f (lambda (x) (lambda (x) x)))
                    ((f #'x') #'y')
                  |}
                ~expected:{| #'y' |})
        ] )
    ]
;;
