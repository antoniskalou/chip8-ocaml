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
  ; screen : Screen.t
  }

let create memory =
  { registers = Array.make 16 Uint8.zero
  ; i = Uint16.zero
  ; pc = Memory.rom_base_address
  (* caml8 uses this address in our local memory, though we can use whatever
      stack-like structure we prefer. *)
  ; sp = Uint16.of_int 0xFA0
  ; screen = Screen.create ~w:64 ~h:32
  ; memory
  }

let screen_buffer (t: t): bool array = Screen.buffer t.screen

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
| Set_vx_to_vy of register * register
| Add of register * uint8
| Add_vx_to_vy of register * register
| Subtract_vy_from_vx of register * register
| Subtract_vx_from_vy of register * register
| Shift_right of register * register
| Shift_left of register * register
| Binary_or of register * register
| Binary_and of register * register
| Binary_xor of register * register
| Jump of uint16
| JumpV0 of uint16
| Call of uint16
| Skip_if_eq of register * uint8
| Skip_if_ne of register * uint8
| Skip_if_vx_vy_eq of register * register
| Skip_if_vx_vy_ne of register * register
| Draw of register * register * uint8

let decode opcode =
  let u8 = Uint8.of_int in
  match Nibbles.of_uint16 opcode with
  | (0x0, 0x0, 0xE, 0x0) -> Clear
  | (0x0, 0x0, 0xE, 0xE) -> Return
  | (0x1, n1, n2, n3) -> Jump (Nibbles.to_uint16 n1 n2 n3)
  | (0x2, n1, n2, n3) -> Call (Nibbles.to_uint16 n1 n2 n3)
  | (0x3, x, n1, n2) -> Skip_if_eq (u8 x, Nibbles.to_uint8 n1 n2)
  | (0x4, x, n1, n2) -> Skip_if_ne (u8 x, Nibbles.to_uint8 n1 n2)
  | (0x5, x, y, 0x0) -> Skip_if_vx_vy_eq (u8 x, u8 y)
  | (0x6, x, n1, n2) -> Set (u8 x, Nibbles.to_uint8 n1 n2)
  | (0x7, x, n1, n2) -> Add (u8 x, Nibbles.to_uint8 n1 n2)
  | (0x8, x, y, 0x0) -> Set_vx_to_vy (u8 x, u8 y)
  | (0x8, x, y, 0x1) -> Binary_or (u8 x, u8 y)
  | (0x8, x, y, 0x2) -> Binary_and (u8 x, u8 y)
  | (0x8, x, y, 0x3) -> Binary_xor (u8 x, u8 y)
  | (0x8, x, y, 0x4) -> Add_vx_to_vy (u8 x, u8 y)
  | (0x8, x, y, 0x5) -> Subtract_vy_from_vx (u8 x, u8 y)
  | (0x8, x, y, 0x6) -> Shift_right (u8 x, u8 y)
  | (0x8, x, y, 0x7) -> Subtract_vx_from_vy (u8 x, u8 y)
  | (0x8, x, y, 0xE) -> Shift_left (u8 x, u8 y)
  | (0x9, x, y, 0x0) -> Skip_if_vx_vy_ne (u8 x, u8 y)
  | (0xA, n1, n2, n3) -> Set_index (Nibbles.to_uint16 n1 n2 n3)
  | (0xB, n1, n2, n3) -> JumpV0 (Nibbles.to_uint16 n1 n2 n3)
  | (0xD, x, y, n) -> Draw (u8 x, Uint8.of_int y, Uint8.of_int n)
  | _ ->
    let opcode_str = Printf.sprintf "0x%04X" (Uint16.to_int opcode) in
    raise (Unknown_opcode (opcode_str, opcode))

let execute t instruction =
  let read_register v = t.registers.(Uint8.to_int v) in
  let write_register v x = t.registers.(Uint8.to_int v) <- x in
  let skip () = t.pc <- Uint16.(t.pc + of_int 2) in
  match instruction with
  | Clear -> Array.fill t.screen 0 (Array.length t.screen) false
  | Set (vx, value) ->
    write_register vx value;
  | Set_index (i) ->
    t.i <- i
  | Set_vx_to_vy (vx, vy) ->
    let y = read_register vy in
    write_register vx y
  | Add (vx, value) ->
    let x = read_register vx in
    write_register vx Uint8.(x + value)
  | Add_vx_to_vy (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx Uint8.(x + y);
    (* check if integer has overflowed *)
    if read_register vx < x
    then t.registers.(0xF) <- Uint8.one
    else t.registers.(0xF) <- Uint8.zero
  | Subtract_vy_from_vx (vx, vy)
  | Subtract_vx_from_vy (vy, vx) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx Uint8.(x - y);
    (* only set VF when value underflows *)
    if x < y
    then t.registers.(0xF) <- Uint8.one
    else t.registers.(0xF) <- Uint8.zero
  | Shift_right (vx, vy) ->
    let y = read_register vy in
    write_register vx (Uint8.shift_right y 1);
    t.registers.(0xF) <- Uint8.logand y Uint8.one;
  | Shift_left (vx, vy) ->
    let y = read_register vy in
    write_register vx (Uint8.shift_left y 1);
    t.registers.(0xF) <- Uint8.logand y (Uint8.of_int 0b1000_0000);
  | Binary_or (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx (Uint8.logor x y);
  | Binary_and (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx (Uint8.logand x y);
  | Binary_xor (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx (Uint8.logxor x y);
  | Draw (vx, vy, rows) ->
    let x = read_register vx |> Uint8.to_int in
    let y = read_register vy |> Uint8.to_int in
    let f_flag =
      Screen.draw t.screen ~memory:t.memory ~i:t.i ~vx:x ~vy:y ~rows
    in
    t.registers.(0xF) <- f_flag;
  | Skip_if_eq (vx, value) ->
    let x = read_register vx in
    if x = value then skip () else ()
  | Skip_if_ne (vx, value) ->
    let x = read_register vx in
    if x != value then skip () else ()
  | Skip_if_vx_vy_eq (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    if x = y then skip () else ()
  | Skip_if_vx_vy_ne (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    if x != y then skip () else ()
  | Jump addr ->
    t.pc <- addr
  | JumpV0 addr ->
    t.pc <- Uint16.(addr + (of_uint8 t.registers.(0x0)))
  | Call addr ->
    t.sp <- Uint16.(t.sp + of_int 2);
    Memory.write_uint16 t.memory ~pos:t.sp t.pc;
    t.pc <- addr
  | Return ->
    let addr = Memory.read_uint16 t.memory ~pos:t.sp in
    t.sp <- Uint16.(t.sp - of_int 2);
    t.pc <- addr

let tick t =
  fetch t
  |> decode
  |> execute t
