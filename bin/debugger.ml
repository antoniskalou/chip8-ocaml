open Stdint
open Chip8

let prompt s =
  Printf.printf "%s %!" s;
  In_channel.input_line stdin

type cmd =
  | Run
  | Step
  | List
  (* | Breakpoint of uint16 *)
  | Print_registers

let parse_command cmd_str =
  match cmd_str with
  | "r" | "run" -> Some Run
  | "s" | "step" -> Some Step
  | "l" | "list" -> Some List
  (* | "b" | "break" -> Some (Breakpoint) *)
  | "p" | "print" -> Some Print_registers
  | _ -> None

let run_until_breakpoint cpu =
  while true do
    Cpu.tick cpu
  done

let step_execution = Cpu.tick

let list_code ~(pc: uint16) ~memory =
  let pc = Uint16.to_int pc in
  (* number of opcodes to traverse, x2 because its 16 bit *)
  let n_opcodes = 5 * 2 in
  let distance_start = pc - Uint16.to_int Memory.rom_base_address in
  let distance_finish = Memory.size memory - pc in
  let (start, finish) =
    (* we're at the start so we can't go backward, show 10 forward instead *)
    if distance_start < n_opcodes then
      (* reduce effect of looking backwards as we move up the rom *)
      (pc - distance_start, (pc + n_opcodes * 2) - distance_start)
    (* we're at the end of memory, go 10 backward *)
    else if distance_finish < n_opcodes then
      (pc - n_opcodes * 2, pc - distance_finish)
    (* we can view memory both forward and backward *)
    else (pc - n_opcodes, pc + n_opcodes)
  in
  let i = ref start in
  while !i < finish do
    memory
    |> Memory.read_uint16 ~pos:(Uint16.of_int !i)
    |> Uint16.to_int
    |> Printf.printf "%04X: %04X" !i;
    (* show where we are in execution *)
    if !i = pc then begin
      Printf.printf " <--"
    end;
    Printf.printf "\n%!";
    i := !i + 2
  done

let print_registers (cpu: Cpu.t) =
  cpu.registers
  |> Array.iteri (fun i r ->
      Printf.printf "V%i = %02X\n%!" i (Uint8.to_int r));
  Printf.printf "I = %04X\n%!" (Uint16.to_int cpu.i)

let execute_command ~cpu =
  function
  | Run -> run_until_breakpoint cpu
  | Step -> step_execution cpu
  | List -> list_code ~pc:cpu.pc ~memory:cpu.memory
  | Print_registers -> print_registers cpu

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  let rom = Rom.load argv.(1) in
  let memory = Memory.create () in
  Memory.load memory ~src:rom ~pos:Memory.rom_base_address;
  let cpu = Cpu.create memory in
  while true do
    match prompt ">" with
    | Some cmd_str ->
      (match parse_command cmd_str with
      | Some cmd -> execute_command cmd ~cpu
      | None -> Printf.eprintf "Unknown command %s\n%!" cmd_str)
    | None ->
      Printf.eprintf "Bye!\n";
      exit 0
  done
