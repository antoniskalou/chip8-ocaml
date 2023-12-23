open Stdint

type t =
  { buffer : bool array
  ; width : int
  ; height : int
  }

(* TODO: ensure w and h are powers of 2 and have a 2:1 ratio *)
let create ~w ~h =
  { buffer = Array.make (w * h) false
  ; width = w
  ; height = h
  }

let buffer (t: t): bool array = t.buffer

let clear { buffer; _ } =
  Array.fill buffer 0 (Array.length buffer) false

let draw t ~memory ~i ~x ~y ~rows =
  let f_flag = ref Uint8.zero in
  for y_line = 0 to Uint8.to_int rows - 1 do
    let pixels =
      Memory.read_uint8 memory ~pos:Uint16.(i + of_int y_line)
      |> Uint8.to_int
    in
    for x_line = 0 to 7 do
      if (pixels land (0b1000_0000 lsr x_line)) <> 0 then begin
        (* TODO: get screen w/h from t *)
        let x' = (x + x_line) mod t.width in
        let y' = (y + y_line) mod t.height in
        (* pixel index in 1d screen array *)
        let idx = x' + t.width * y' in
        if t.buffer.(idx) then begin
          t.buffer.(idx) <- false;
          f_flag := Uint8.one
        end else t.buffer.(idx) <- true
      end
    done
  done;
  !f_flag
