open Stdint
open Chip8

let prompt s =
  Printf.printf "%s %!" s;
  In_channel.input_line stdin

type cmd =
  | Run
  | Step
  | List_code of uint16 option
  | Memset of uint16 * uint16
  (* | Breakpoint of uint16 *)
  | Print_registers

let parse_command cmd_str =
  match String.split_on_char ' ' cmd_str with
  | ["r"] | ["run"] -> Some Run
  | ["s"] | ["step"] -> Some Step
  | ["l"] | ["list"] -> Some (List_code None)
  | ["l"; look_at] | ["list"; look_at] ->
    Some (List_code (Some (Uint16.of_string look_at)))
  | ["p"] | ["print"] -> Some Print_registers
  | ["memset"; addr; value] ->
    (* TODO: handle errors *)
    Some (Memset (Uint16.of_string addr, Uint16.of_string value))
  | _ -> None

let run_until_breakpoint cpu =
  while true do
    Cpu.tick cpu
  done

let step_execution = Cpu.tick

let list_code ~(look_at: uint16) ~(pc: uint16) ~memory =
  let look_at = Uint16.to_int look_at in
  (* number of opcodes to traverse, x2 because its 16 bit *)
  let n_opcodes = 5 * 2 in
  let distance_finish = Memory.size memory - look_at in
  let (start, finish) =
    (* we're at the start so we can't go backward, show 10 forward instead *)
    if look_at < n_opcodes then
      (* reduce effect of looking backwards as we move up the rom *)
      (look_at, (look_at + n_opcodes * 2) - look_at)
    (* we're at the end of memory, go 10 backward *)
    else if distance_finish < n_opcodes then
      (look_at - n_opcodes * 2, look_at - distance_finish)
    (* we can view memory both forward and backward *)
    else (look_at - n_opcodes, look_at + n_opcodes)
  in
  let i = ref start in
  while !i < finish do
    memory
    |> Memory.read_uint16 ~pos:(Uint16.of_int !i)
    |> Uint16.to_int
    |> Printf.printf "%04X: %04X" !i;
    (* show where we are in execution *)
    if !i = (Uint16.to_int pc) then begin
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

let set_value_in_memory ~memory (addr: uint16) (value: uint16) =
  Memory.write_uint16 memory ~pos:addr value

let execute_command ~cpu =
  function
  | Run -> run_until_breakpoint cpu
  | Step -> step_execution cpu
  | List_code look_at_opt ->
    let look_at = Option.value ~default:cpu.pc look_at_opt in
    list_code ~look_at ~pc:cpu.pc ~memory:cpu.memory
  | Print_registers -> print_registers cpu
  | Memset (addr, value) ->
    set_value_in_memory ~memory:cpu.memory addr value

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
