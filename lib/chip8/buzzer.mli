type config = { volume : float; frequency : float }
type t

val create : config -> (t, [`Msg of string]) result

val play : t -> unit

val pause : t -> unit

(* val stop : t -> unit *)
