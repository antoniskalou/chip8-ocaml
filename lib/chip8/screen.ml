open Stdint

type t = bool array

(* TODO: ensure w and h are powers of 2 and have a 2:1 ratio *)
let create ~w ~h = Array.make (w * h) false

let buffer (t: t): bool array = t

let draw t ~memory ~i ~vx ~vy ~rows =
  let f_flag = ref Uint8.zero in
  for y = 0 to Uint8.to_int rows - 1 do
    let line =
      Memory.read_uint8 memory ~pos:Uint16.(i + of_int y)
      |> Uint8.to_int
    in
    for x = 0 to 7 do
      let bit = (line lsr (7 - x)) land (0b00000001) = 1 in
      let screen_idx = ((y + vy) mod 32) * 64 + (x + (vx mod 64)) in
      if bit && t.(screen_idx) then begin
        t.(screen_idx) <- false;
        f_flag := Uint8.one;
      end else if bit then
        t.(screen_idx) <- true
    done
  done;
  !f_flag
