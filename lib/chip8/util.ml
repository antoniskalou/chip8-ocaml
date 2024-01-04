(* return the amount of time it takes to execute f in seconds *)
let timed f =
  let start = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. start

(* backported from OCaml 5 *)

let find_index p a =
  let open Array in
  let n = length a in
  let rec loop i =
    if i = n then None
    else if p (get a i) then Some i
    else loop (succ i)
  in
  loop 0

let [@tail_mod_cons] rec input_lines ic =
  match Stdlib.input_line ic with
  | line -> line :: input_lines ic
  | exception End_of_file -> []
