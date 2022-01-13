open Lib

let value = Alcotest.testable (Fmt.of_to_string Value.to_string) Value.equal