type value

type env

val env_to_string : env -> string

val run : env -> Sexpr.t -> (env * value, string) result

val run_all : env -> Sexpr.t list -> (env * value list, string) result

val initial_env : env

val value_to_string : value -> string

val run_file : string -> (env * value, string) result