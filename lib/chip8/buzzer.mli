type t

val create
  : volume:float
  -> frequency:float
  -> (t, [`Msg of string]) result

val play : t -> unit

val pause : t -> unit

(* val stop : t -> unit *)
