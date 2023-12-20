open Stdint

let uint8_of_nibbles (n1: int) (n2: int): uint8 =
  Uint8.of_int ((n1 lsl 4) + n2)

let uint16_of_nibbles (n1: int) (n2: int) (n3: int): uint16 =
  Uint16.of_int ((n1 lsl 8) + (n2 lsl 4) + n3)

let nibbles_of_uint16 (x : uint16) : int * int * int * int =
  let x = Uint16.to_int x in
  (
    (* x >> 12 *)
    x lsr 12,
    (* (0x0F00 & x) >> 8 *)
    (0x0F00 land x) lsr 8,
    (* (0x00F0 & x) >> 4 *)
    (0x00F0 land x) lsr 4,
    (* 0x000F & x *)
    0x000F land x
  )

let string_of_opcode opcode =
  let nn_to_string n1 n2 =
    uint8_of_nibbles n1 n2
    |> Uint8.to_int
    |> Printf.sprintf "%X"
  in
  let nnn_to_string n1 n2 n3 =
    uint16_of_nibbles n1 n2 n3
    |> Uint16.to_int
    |> Printf.sprintf "%X"
  in
  (* see http://johnearnest.github.io/Octo/docs/chip8ref.pdf *)
  match nibbles_of_uint16 opcode with
  | (0x0, 0x0, 0xE, 0x0) -> "CLEAR"
  | (0x0, 0x0, 0xE, 0xE) -> "RETURN"
  | (0x1, n1, n2, n3) -> Printf.sprintf "JUMP %s" (nnn_to_string n1 n2 n3)
  | (0x2, n1, n2, n3) -> Printf.sprintf "CALL %s" (nnn_to_string n1 n2 n3)
  | (0x3, x, n1, n2) -> Printf.sprintf "IF V%i != %s THEN" x (nn_to_string n1 n2)
  | (0x4, x, n1, n2) -> Printf.sprintf "IF V%i == %s THEN" x (nn_to_string n1 n2)
  | (0x5, x, y, 0x0) -> Printf.sprintf "IF V%i != V%i THEN" x y
  | (0x6, x, n1, n2) -> Printf.sprintf "V%i := %s" x (nn_to_string n1 n2)
  | (0x7, x, n1, n2) -> Printf.sprintf "V%i += %s" x (nn_to_string n1 n2)
  | (0x8, x, y, 0x0) -> Printf.sprintf "V%i := V%i" x y
  | (0x8, x, y, 0x1) -> Printf.sprintf "V%i |= V%i" x y
  | (0x8, x, y, 0x2) -> Printf.sprintf "V%i &= V%i" x y
  | (0x8, x, y, 0x3) -> Printf.sprintf "V%i ^= V%i" x y
  | (0x8, x, y, 0x4) -> Printf.sprintf "V%i += V%i" x y
  | (0x8, x, y, 0x5) -> Printf.sprintf "V%i -= V%i" x y
  | (0x8, x, y, 0x6) -> Printf.sprintf "V%i >>= V%i" x y
  | (0x8, x, y, 0x7) -> Printf.sprintf "V%i =- V%i" x y
  | (0x8, x, y, 0xE) -> Printf.sprintf "V%i <<= V%i" x y
  | (0x9, x, y, 0x0) -> Printf.sprintf "IF V%i == V%i THEN" x y
  | (0xA, n1, n2, n3) -> Printf.sprintf "I := %s" (nnn_to_string n1 n2 n3)
  | (0xB, n1, n2, n3) -> Printf.sprintf "JUMP0 %s" (nnn_to_string n1 n2 n3)
  | (0xC, x, n1, n2) -> Printf.sprintf "V%i := RANDOM %s" x (nn_to_string n1 n2)
  | (0xD, x, y, n) -> Printf.sprintf "DRAW V%i V%i %i" x y n
  | (0xE, x, 0x9, 0xE) -> Printf.sprintf "IF V%i -KEY THEN" x
  | (0xE, x, 0xA, 0x1) -> Printf.sprintf "IF V%i KEY THEN" x
  | (0xF, x, 0x0, 0x7) -> Printf.sprintf "V%i := DELAY" x
  | (0xF, x, 0x0, 0xA) -> Printf.sprintf "V%i := KEY" x
  | (0xF, x, 0x1, 0x5) -> Printf.sprintf "DELAY := V%i" x
  | (0xF, x, 0x1, 0x8) -> Printf.sprintf "BUZZER := V%i" x
  | (0xF, x, 0x1, 0xE) -> Printf.sprintf "I += V%i" x
  | (0xF, x, 0x2, 0x9) -> Printf.sprintf "I :=  V%i" x
  | (0xF, x, 0x3, 0x3) -> Printf.sprintf "BCD V%i" x
  | (0xF, x, 0x5, 0x5) -> Printf.sprintf "SAVE V%i" x
  | (0xF, x, 0x6, 0x5) -> Printf.sprintf "LOAD V%i" x
  | _ ->
    Printf.sprintf "<UNKNOWN> %04X" (Uint16.to_int opcode)

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  let rom =
    In_channel.with_open_bin argv.(1) In_channel.input_all
    |> Bytes.of_string
  in
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
