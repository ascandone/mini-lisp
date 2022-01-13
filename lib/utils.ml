let string_of_chars lst = String.of_seq @@ List.to_seq lst
let chars_of_string str = List.of_seq @@ String.to_seq str

let arity_error_msg label args =
  let n = Int.to_string (List.length args) in
  "Wrong number of args (" ^ n ^ ") passed to " ^ label
;;

module LetSyntax = struct
  module Result = struct
    let ( let* ) = Result.bind
    let ( let+ ) x y = Result.map y x
  end

  module Option = struct
    let ( let* ) = Option.bind
    let ( let+ ) x y = Option.map y x
  end
end
