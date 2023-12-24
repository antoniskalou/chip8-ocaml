open Stdint
open Chip8

let string_of_opcode opcode =
  let u8i = Uint8.to_int in
  let u16i = Uint16.to_int in
  (* see http://johnearnest.github.io/Octo/docs/chip8ref.pdf *)
  match Cpu.decode opcode with
  | Clear -> "CLEAR"
  | Return -> "RETURN"
  | Jump addr -> Printf.sprintf "JUMP %04X" (u16i addr)
  | Call addr -> Printf.sprintf "CALL %04X" (u16i addr)
  | Skip_if_eq (vx, value) ->
    Printf.sprintf "IF V%X != %02X THEN" (u8i vx) (u8i value)
  | Skip_if_ne (vx, value) ->
    Printf.sprintf "IF V%X == %02X THEN" (u8i vx) (u8i value)
  | Skip_if_vx_vy_eq (vx, vy) ->
    Printf.sprintf "IF V%X != V%X THEN" (u8i vx) (u8i vy)
  | Set (vx, value) ->
    Printf.sprintf "V%X := %02X" (u8i vx) (u8i value)
  | Add (vx, value) ->
    Printf.sprintf "V%X += %02X" (u8i vx) (u8i value)
  | Set_vx_to_vy (vx, vy) ->
    Printf.sprintf "V%X := V%X" (u8i vx) (u8i vy)
  | Binary_or (vx, vy) ->
    Printf.sprintf "V%X |= V%X" (u8i vx) (u8i vy)
  | Binary_and (vx, vy) ->
    Printf.sprintf "V%X &= V%X" (u8i vx) (u8i vy)
  | Binary_xor (vx, vy) ->
    Printf.sprintf "V%X ^= V%X" (u8i vx) (u8i vy)
  | Add_vx_to_vy (vx, vy) ->
    Printf.sprintf "V%X += V%X" (u8i vx) (u8i vy)
  | Subtract_vy_from_vx (vx, vy) ->
    Printf.sprintf "V%X -= V%X" (u8i vx) (u8i vy)
  | Shift_right (vx, vy) ->
    Printf.sprintf "V%X >>= V%X" (u8i vx) (u8i vy)
  | Subtract_vx_from_vy (vx, vy) ->
    Printf.sprintf "V%X =- V%X" (u8i vx) (u8i vy)
  | Shift_left (vx, vy) ->
    Printf.sprintf "V%i <<= V%i" (u8i vx) (u8i vy)
  | Skip_if_vx_vy_ne (vx, vy) ->
    Printf.sprintf "IF V%X == V%X THEN" (u8i vx) (u8i vy)
  | Set_index addr ->
    Printf.sprintf "I := %04X" (u16i addr)
  | JumpV0 addr ->
    Printf.sprintf "JUMP0 %04X" (u16i addr)
  | Random (vx, value) ->
    Printf.sprintf "V%X := RANDOM %02X" (u8i vx) (u8i value)
  | Draw (vx, vy, n) ->
    Printf.sprintf "DRAW V%X V%X %i" (u8i vx) (u8i vy) (u8i n)
  | Skip_if_not_pressed vx ->
    Printf.sprintf "IF V%X -KEY THEN" (u8i vx)
  | Skip_if_pressed vx ->
    Printf.sprintf "IF V%X KEY THEN" (u8i vx)
  | Read_delay vx ->
    Printf.sprintf "V%X := DELAY" (u8i vx)
  | Wait_until_pressed vx ->
    Printf.sprintf "V%X := KEY" (u8i vx)
  | Set_delay vx ->
    Printf.sprintf "DELAY := V%X" (u8i vx)
  | Set_sound vx ->
    Printf.sprintf "BUZZER := V%X" (u8i vx)
  | Add_to_index vx ->
    Printf.sprintf "I += V%X" (u8i vx)
  | Set_font vx ->
    Printf.sprintf "I :=  V%X" (u8i vx)
  | Bcd vx -> Printf.sprintf "BCD V%X" (u8i vx)
  | Store vx -> Printf.sprintf "STORE V%X" (u8i vx)
  | Load vx  -> Printf.sprintf "LOAD V%X" (u8i vx)

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  let rom = Rom.load argv.(1) in
  let pc = ref 0 in
  while !pc < (Bytes.length rom) - 2 do
    let (b1, b2) = (Bytes.get_uint8 rom !pc, Bytes.get_uint8 rom (!pc + 1)) in
    (* b1 << 8 | b2 *)
    let opcode = Uint16.of_int ((b1 lsl 8) lor b2) in
    Printf.printf "%04X: %s\n"
      (!pc + 0x200) (* ROM is expecting to be at address 200 *)
      (string_of_opcode opcode);
    pc := !pc + 2
  done
