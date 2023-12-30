(* return the amount of time it takes to execute f in seconds *)
let timed f =
  let start = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. start
