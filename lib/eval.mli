(* include module type of Eval *)

type value =
  | Number of float
  | Symbol of string
  | List of value list
  | Native of native_function
  | Lambda of (env * string list * value)

and native_function

and env

val env_to_string : env -> string

val run : env -> Sexpr.t -> (env * value, string) result

val run_all : env -> Sexpr.t list -> (env * value list, string) result

val initial_env : env

val value_to_string : value -> string
