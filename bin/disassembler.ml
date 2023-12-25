open Stdint
open Chip8

let string_of_opcode opcode =
  Printf.sprintf "%04X" (Uint16.to_int opcode)

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  let rom = Rom.load argv.(1) in
  let pc = ref 0 in
  while !pc < (Bytes.length rom) do
    let (b1, b2) = (Bytes.get_uint8 rom !pc, Bytes.get_uint8 rom (!pc + 1)) in
    (* b1 << 8 | b2 *)
    let opcode = Uint16.of_int ((b1 lsl 8) lor b2) in
    Printf.printf "%04X: %s\n"
      (!pc + Uint16.to_int Memory.rom_base_address)
      (string_of_opcode opcode);
    pc := !pc + 2
  done
