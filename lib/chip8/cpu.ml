open Stdint

type register = uint8

type t =
  { registers : register array
  ; mutable i : uint16
  ; mutable pc : uint16
  ; mutable sp : uint16
  ; memory : Memory.t
  (* represents a render buffer, when true is encountered a pixel
      is drawn at that coordinate *)
  ; screen : bool array
  }

let create memory =
  { registers = Array.make 16 Uint8.zero
  ; i = Uint16.zero
  ; pc = Memory.rom_base_address
  (* caml8 uses this address in our local memory, though we can use whatever
      stack-like structure we prefer. *)
  ; sp = Uint16.of_int 0xFA0
  ; screen = Array.make (64 * 32) false
  ; memory
  }

let screen_buffer t = t.screen

let fetch (t: t) =
  let opcode = Memory.read_uint16 t.memory ~pos:t.pc in
  (* always increment the program counter, later on we can decide if we want
    to skip or jump. *)
  t.pc <- Uint16.(t.pc + (of_int 2));
  opcode

exception Unknown_opcode of string * uint16

type instruction =
| Clear
| Return
| Set of register * uint8
| Set_index of uint16
| Add of register * uint8
| Jump of uint16
| Jump0 of uint16
| Call of uint16
| Draw of register * register * uint8

let decode opcode =
  match Nibbles.of_uint16 opcode with
  | (0x0, 0x0, 0xE, 0x0) -> Clear
  | (0x0, 0x0, 0xE, 0xE) -> Return
  | (0x1, n1, n2, n3) -> Jump (Nibbles.to_uint16 n1 n2 n3)
  | (0x2, n1, n2, n3) -> Call (Nibbles.to_uint16 n1 n2 n3)
  | (0x6, x, n1, n2) -> Set (Uint8.of_int x, Nibbles.to_uint8 n1 n2)
  | (0x7, x, n1, n2) -> Add (Uint8.of_int x, Nibbles.to_uint8 n1 n2)
  | (0xA, n1, n2, n3) -> Set_index (Nibbles.to_uint16 n1 n2 n3)
  | (0xB, n1, n2, n3) -> Jump0 (Nibbles.to_uint16 n1 n2 n3)
  | (0xD, x, y, n) -> Draw (Uint8.of_int x, Uint8.of_int y, Uint8.of_int n)
  | _ ->
    let opcode_str = Printf.sprintf "0x%04X" (Uint16.to_int opcode) in
    raise (Unknown_opcode (opcode_str, opcode))

let execute t instruction =
  match instruction with
  | Clear -> Array.fill t.screen 0 (Array.length t.screen) false
  | Set (vx, x) ->
    t.registers.(Uint8.to_int vx) <- x
  | Set_index (i) ->
    t.i <- i
  | Add (vx, x) ->
    let vx' = t.registers.(Uint8.to_int vx) in
    t.registers.(Uint8.to_int vx) <- Uint8.(x + vx')
  | Draw (vx, vy, rows) ->
    let vx = t.registers.(Uint8.to_int vx) |> Uint8.to_int in
    let vy = t.registers.(Uint8.to_int vy) |> Uint8.to_int in
    t.registers.(0xF) <- Uint8.zero;
    for y = 0 to Uint8.to_int rows - 1 do
      let line =
        Memory.read_uint8 t.memory ~pos:Uint16.(t.i + of_int y)
        |> Uint8.to_int
      in
      for x = 0 to 7 do
        let bit = (line lsr (7 - x)) land (0b00000001) = 1 in
        let screen_idx = ((y + vy) mod 32) * 64 + (x + (vx mod 64)) in
        if bit && t.screen.(screen_idx) then begin
          t.screen.(screen_idx) <- false;
          t.registers.(0xF) <- Uint8.one;
        end else if bit then
          t.screen.(screen_idx) <- true
      done
    done;
    ()
  | Jump tgt -> t.pc <- tgt
  | Return -> failwith "TODO"
  | Jump0 _ -> failwith "TODO"
  | Call _ -> failwith "TODO"

let tick t =
  fetch t
  |> decode
  |> execute t
