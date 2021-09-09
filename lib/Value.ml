module StringMap = Map.Make (String)

type env_item = Value of t | Macro of t list * t

and t =
  | Number of float
  | Symbol of string
  | Char of char
  | List of t list
  | Native of native_function
  | Lambda of (env * t list * t)

and native_function = t list -> (t, string) result

and env = env_item StringMap.t