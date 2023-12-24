open Stdint
open Chip8

let string_of_opcode opcode =
  let u8i = Uint8.to_int in
  let u16i = Uint16.to_int in
  (* see http://devernay.free.fr/hacks/chip8/C8TECH10.HTM for syntax,
     i have made minor adjustments to make it easier to parse.

     I don't use commas and I also rename some commands to be easier
     to read, e.g. LD F VX to LD FONT VX*)
  match Cpu.decode opcode with
  | Clear -> "CLS"
  | Return -> "RET"
  | Jump addr -> Printf.sprintf "JP %04X" (u16i addr)
  | Call addr -> Printf.sprintf "CALL %04X" (u16i addr)
  | Skip_if_eq (vx, value) ->
    Printf.sprintf "SE V%X %02X" (u8i vx) (u8i value)
  | Skip_if_ne (vx, value) ->
    Printf.sprintf "SNE V%X %02X" (u8i vx) (u8i value)
  | Skip_if_vx_vy_eq (vx, vy) ->
    Printf.sprintf "SE V%X V%X" (u8i vx) (u8i vy)
  | Set (vx, value) ->
    Printf.sprintf "LD V%X %02X" (u8i vx) (u8i value)
  | Add (vx, value) ->
    Printf.sprintf "ADD V%X %02X" (u8i vx) (u8i value)
  | Set_vx_to_vy (vx, vy) ->
    Printf.sprintf "LD V%X V%X" (u8i vx) (u8i vy)
  | Binary_or (vx, vy) ->
    Printf.sprintf "OR V%X V%X" (u8i vx) (u8i vy)
  | Binary_and (vx, vy) ->
    Printf.sprintf "AND V%X V%X" (u8i vx) (u8i vy)
  | Binary_xor (vx, vy) ->
    Printf.sprintf "XOR V%X V%X" (u8i vx) (u8i vy)
  | Add_vx_to_vy (vx, vy) ->
    Printf.sprintf "ADD V%X V%X" (u8i vx) (u8i vy)
  | Subtract_vy_from_vx (vx, vy) ->
    Printf.sprintf "SUB V%X V%X" (u8i vx) (u8i vy)
  | Shift_right (vx, vy) ->
    Printf.sprintf "SHR V%X V%X" (u8i vx) (u8i vy)
  | Subtract_vx_from_vy (vx, vy) ->
    Printf.sprintf "SUBN V%X V%X" (u8i vx) (u8i vy)
  | Shift_left (vx, vy) ->
    Printf.sprintf "SHL V%i V%i" (u8i vx) (u8i vy)
  | Skip_if_vx_vy_ne (vx, vy) ->
    Printf.sprintf "SNE V%X V%X" (u8i vx) (u8i vy)
  | Set_index addr ->
    Printf.sprintf "LD I %04X" (u16i addr)
  | JumpV0 addr ->
    Printf.sprintf "JPV0 %04X" (u16i addr)
  | Random (vx, value) ->
    Printf.sprintf "RND V%X %02X" (u8i vx) (u8i value)
  | Draw (vx, vy, n) ->
    Printf.sprintf "DRW V%X V%X %i" (u8i vx) (u8i vy) (u8i n)
  | Skip_if_not_pressed vx ->
    Printf.sprintf "SKP V%X" (u8i vx)
  | Skip_if_pressed vx ->
    Printf.sprintf "SKNP V%X" (u8i vx)
  | Read_delay vx ->
    Printf.sprintf "LD V%X DELAY" (u8i vx)
  | Wait_until_pressed vx ->
    Printf.sprintf "LD V%X KEY" (u8i vx)
  | Set_delay vx ->
    Printf.sprintf "LD DELAY V%X" (u8i vx)
  | Set_sound vx ->
    Printf.sprintf "LD SOUND V%X" (u8i vx)
  | Add_to_index vx ->
    Printf.sprintf "ADD I V%X" (u8i vx)
  | Set_font vx ->
    Printf.sprintf "LD FONT V%X" (u8i vx)
  | Bcd vx -> Printf.sprintf "LD BCD V%X" (u8i vx)
  | Store vx -> Printf.sprintf "LD [I] V%X" (u8i vx)
  | Load vx  -> Printf.sprintf "LD V%X [I]" (u8i vx)

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
    let opcode_s =
      try string_of_opcode opcode
      with Cpu.Unknown_opcode (_, opcode) ->
        Printf.sprintf "%04X" (Uint16.to_int opcode)
    in
    Printf.printf "%s\n" opcode_s;
    pc := !pc + 2
  done
