let string_of_chars lst = String.of_seq @@ List.to_seq lst

let chars_of_string str = List.of_seq @@ String.to_seq str
