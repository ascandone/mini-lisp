val run : Value.env -> Value.t -> (Value.env * Value.t, string) result

val run_all :
  ?debug_read:bool ->
  Value.env ->
  Value.t list ->
  (Value.env * Value.t list, string) result

val initial_env : Value.env

val run_file : string -> (Value.env * Value.t, string) result