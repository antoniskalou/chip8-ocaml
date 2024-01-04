open Stdint

type register = uint8

type t =
  { registers : register array
  ; mutable i : uint16
  ; mutable pc : uint16
  ; mutable sp : uint16
  ; mutable delay : uint8
  ; mutable sound : uint8
  ; keys : bool array
  ; memory : Memory.t
  (* represents a render buffer, when true is encountered a pixel
      is drawn at that coordinate *)
  ; screen : Screen.t
  }

let create memory =
  { registers = Array.make 16 Uint8.zero
  ; i = Uint16.zero
  ; pc = Memory.rom_base_address
  ; sp = Uint16.of_int 0xFA0
  ; delay = Uint8.zero
  ; sound = Uint8.zero
  ; keys = Array.make 16 false
  (* caml8 uses this address in our local memory, though we can use whatever
      stack-like structure we prefer. *)
  ; screen = Screen.create ~w:64 ~h:32
  ; memory
  }

let screen_buffer (t: t): bool array = Screen.buffer t.screen

let press_key (t: t) (key: int) (pressed: bool) =
  t.keys.(key) <- pressed

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
| Set_delay of register
| Set_sound of register
| Set_vx_to_vy of register * register
| Set_font of register
| Read_delay of register
| Random of register * uint8
| Add of register * uint8
| Add_to_index of register
| Add_vx_to_vy of register * register
| Subtract_vy_from_vx of register * register
| Subtract_vx_from_vy of register * register
| Shift_right of register * register
| Shift_left of register * register
| Binary_or of register * register
| Binary_and of register * register
| Binary_xor of register * register
| Bcd of register
| Jump of uint16
| JumpV0 of uint16
| Call of uint16
| Skip_if_eq of register * uint8
| Skip_if_ne of register * uint8
| Skip_if_vx_vy_eq of register * register
| Skip_if_vx_vy_ne of register * register
| Skip_if_pressed of register
| Skip_if_not_pressed of register
| Draw of register * register * uint8
| Load of register
| Store of register
| Wait_until_pressed of register

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
  | (0xC, x, n1, n2) -> Random (u8 x, Nibbles.to_uint8 n1 n2)
  | (0xD, x, y, n) -> Draw (u8 x, Uint8.of_int y, Uint8.of_int n)
  | (0xE, x, 0xA, 0x1) -> Skip_if_not_pressed (u8 x)
  | (0xE, x, 0x9, 0xE) -> Skip_if_pressed (u8 x)
  | (0xF, x, 0x0, 0x7) -> Read_delay (u8 x)
  | (0xF, x, 0x0, 0xA) -> Wait_until_pressed (u8 x)
  | (0xF, x, 0x1, 0x5) -> Set_delay (u8 x)
  | (0xF, x, 0x1, 0x8) -> Set_sound (u8 x)
  | (0xF, x, 0x1, 0xE) -> Add_to_index (u8 x)
  | (0xF, x, 0x2, 0x9) -> Set_font (u8 x)
  | (0xF, x, 0x3, 0x3) -> Bcd (u8 x)
  | (0xF, x, 0x5, 0x5) -> Store (u8 x)
  | (0xF, x, 0x6, 0x5) -> Load (u8 x)
  | _ ->
    let opcode_str = Printf.sprintf "0x%04X" (Uint16.to_int opcode) in
    raise (Unknown_opcode (opcode_str, opcode))

let execute t instruction =
  let read_register v = t.registers.(Uint8.to_int v) in
  let write_register v x = t.registers.(Uint8.to_int v) <- x in
  let skip () = t.pc <- Uint16.(t.pc + of_int 2) in
  match instruction with
  | Clear -> Screen.clear t.screen
  | Set (vx, value) ->
    write_register vx value;
  | Set_index i ->
    t.i <- i
  | Set_delay vx ->
    t.delay <- read_register vx
  | Set_sound vx ->
    t.sound <- read_register vx
  | Set_vx_to_vy (vx, vy) ->
    let y = read_register vy in
    write_register vx y
  | Set_font vx ->
    let x = read_register vx in
    t.i <- Uint16.((of_uint8 x) * (of_int 5))
  | Read_delay vx ->
    write_register vx t.delay
  | Random (vx, value) ->
    (* 0x00 to 0xFF inclusive *)
    let rand = Random.int (0xFF + 1) |> Uint8.of_int in
    write_register vx (Uint8.logand rand value)
  | Add (vx, value) ->
    let x = read_register vx in
    write_register vx Uint8.(x + value)
  | Add_to_index vx ->
    t.i <- Uint16.(t.i + of_uint8 (read_register vx));
    (* emulate amiga behaviour in hopes of more game support,
       see https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#fx1e-add-to-index *)
    if t.i >= Uint16.of_int 0x1000
    then t.registers.(0xF) <- Uint8.one;
  | Add_vx_to_vy (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx Uint8.(x + y);
    (* check if integer has overflowed *)
    if read_register vx < x
    then t.registers.(0xF) <- Uint8.one
    else t.registers.(0xF) <- Uint8.zero
  | Subtract_vy_from_vx (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx Uint8.(x - y);
    if Uint8.compare x y <> -1
    then t.registers.(0xF) <- Uint8.one
    else t.registers.(0xF) <- Uint8.zero
  | Subtract_vx_from_vy (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    write_register vx Uint8.(y - x);
    if Uint8.compare y x <> -1
    then t.registers.(0xF) <- Uint8.one
    else t.registers.(0xF) <- Uint8.zero
  | Shift_right (vx, vy) ->
    let y = read_register vy in
    write_register vx (Uint8.shift_right y 1);
    t.registers.(0xF) <- Uint8.logand y Uint8.one;
  | Shift_left (vx, vy) ->
    let y = read_register vy in
    write_register vx (Uint8.shift_left y 1);
    t.registers.(0xF) <- Uint8.shift_right y 7
  | Binary_or (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    t.registers.(0xF) <- Uint8.zero;
    write_register vx (Uint8.logor x y);
  | Binary_and (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    t.registers.(0xF) <- Uint8.zero;
    write_register vx (Uint8.logand x y);
  | Binary_xor (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    t.registers.(0xF) <- Uint8.zero;
    write_register vx (Uint8.logxor x y);
  | Bcd vx ->
    let x = Uint8.to_int (read_register vx) in
    let (ones, tens, hundreds) = (x mod 10, (x / 10) mod 10, (x / 100) mod 10) in
    Memory.write_uint8 t.memory ~pos:t.i (Uint8.of_int hundreds);
    Memory.write_uint8 t.memory ~pos:Uint16.(t.i + (of_int 1)) (Uint8.of_int tens);
    Memory.write_uint8 t.memory ~pos:Uint16.(t.i + (of_int 2)) (Uint8.of_int ones)
  | Draw (vx, vy, rows) ->
    let x = read_register vx |> Uint8.to_int in
    let y = read_register vy |> Uint8.to_int in
    let rows = Uint8.to_int rows in
    let f_flag =
      Screen.draw t.screen ~memory:t.memory ~i:t.i ~x ~y ~rows
    in
    t.registers.(0xF) <- f_flag;
  | Skip_if_eq (vx, value) ->
    let x = read_register vx in
    if x = value then skip () else ()
  | Skip_if_ne (vx, value) ->
    let x = read_register vx in
    if x <> value then skip () else ()
  | Skip_if_vx_vy_eq (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    if x = y then skip () else ()
  | Skip_if_vx_vy_ne (vx, vy) ->
    let x = read_register vx in
    let y = read_register vy in
    if x <> y then skip () else ()
  | Skip_if_pressed vx ->
    let x = read_register vx in
    let key = t.keys.(Uint8.to_int x) in
    if key then t.pc <- Uint16.(t.pc + of_int 2)
  | Skip_if_not_pressed vx ->
    let x = read_register vx in
    let key = t.keys.(Uint8.to_int x) in
    if not key then t.pc <- Uint16.(t.pc + of_int 2)
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
  | Load vx ->
    assert Uint8.(vx < (of_int 0x10));
    Array.sub t.registers 0 (Uint8.to_int vx + 1)
    |> Array.iteri (fun n _ ->
      let n = Uint16.of_int n in
      let x = Memory.read_uint8 t.memory ~pos:Uint16.(t.i + n) in
      write_register (Uint16.to_uint8 n) x)
  | Store vx ->
    assert Uint8.(vx < (of_int 0x10));
    Array.sub t.registers 0 (Uint8.to_int vx + 1)
    |> Array.iteri (fun n x ->
      let pos = Uint16.(t.i + (of_int n)) in
      Memory.write_uint8 t.memory ~pos x)
  | Wait_until_pressed vx ->
    (match Util.find_index Fun.id t.keys with
    | Some i -> write_register vx (Uint8.of_int i)
    (* keep looping *)
    | None -> t.pc <- Uint16.(t.pc - of_int 2))

let sound_playing t =
  Uint8.compare t.sound (Uint8.of_int 2) = 1

(* update game timers, this is expected to be called at 60MHz *)
let update_timers t =
  (* FIXME: this doesn't time correctly, depends on mainloop
     running at exactly 60Hz *)
  if Uint8.compare t.delay Uint8.zero = 1 then
    t.delay <- Uint8.pred t.delay;
  if Uint8.compare t.sound Uint8.zero = 1 then
    t.sound <- Uint8.pred t.sound

let tick t =
  fetch t
  |> decode
  |> execute t
