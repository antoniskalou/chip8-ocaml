open Stdint
open Chip8

let prompt s =
  Printf.printf "%s %!" s;
  In_channel.input_line stdin

type debug_state =
  { mutable breakpoints : uint16 list }

type cmd =
  | Run
  | Step
  | Breakpoint of uint16
  | Memset of uint16 * uint16
  | List_code of uint16 option
  | List_breakpoints
  | Print_registers

let parse_command cmd_str =
  match String.split_on_char ' ' cmd_str with
  | ["r"] | ["run"] -> Some Run
  | ["s"] | ["n"] | ["step"] -> Some Step
  | ["p"] | ["print"] -> Some Print_registers
  | ["l"] | ["list"] -> Some (List_code None)
  | ["l"; look_at] | ["list"; look_at] ->
    Some (List_code (Some (Uint16.of_string look_at)))
  | ["b"] | ["break"] ->
    Some List_breakpoints
  | ["b"; addr] | ["break"; addr] ->
    Some (Breakpoint (Uint16.of_string addr))
  | ["memset"; addr; value] ->
    (* TODO: handle errors *)
    Some (Memset (Uint16.of_string addr, Uint16.of_string value))
  | _ -> None

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

let rec run_until_breakpoint ~(cpu: Cpu.t) ~debug_state =
  if List.exists (fun b -> b = cpu.pc) debug_state.breakpoints
  then Printf.printf "Breakpoint reached.\n%!"
  else begin
    Cpu.tick cpu;
    run_until_breakpoint ~cpu ~debug_state
  end

let step_execution = Cpu.tick

let print_registers (cpu: Cpu.t) =
  cpu.registers
  |> Array.iteri (fun i r ->
      Printf.printf "V%i = %02X\n%!" i (Uint8.to_int r));
  Printf.printf "I = %04X\n%!" (Uint16.to_int cpu.i)

let set_value_in_memory ~memory (addr: uint16) (value: uint16) =
  Memory.write_uint16 memory ~pos:addr value

let execute_command ~cpu ~debug_state =
  function
  | Run -> run_until_breakpoint ~cpu ~debug_state
  | Step -> step_execution cpu
  | Print_registers -> print_registers cpu
  | List_code look_at_opt ->
    let look_at = Option.value ~default:cpu.pc look_at_opt in
    list_code ~look_at ~pc:cpu.pc ~memory:cpu.memory
  | List_breakpoints ->
    if List.is_empty debug_state.breakpoints then
      Printf.printf "No breakpoints set.\n%!"
    else (
      debug_state.breakpoints
      |> List.sort Uint16.compare
      |> List.iteri (fun i b ->
          Printf.printf "%i: %04X\n%!" i (Uint16.to_int b))
    )
  | Breakpoint addr ->
    (* TODO: ensure breakpoint is actually byte aligned *)
    debug_state.breakpoints <- addr :: debug_state.breakpoints
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
  let debug_state = { breakpoints = [] } in
  while true do
    match prompt ">" with
    | Some cmd_str ->
      (match parse_command cmd_str with
      | Some cmd -> execute_command cmd ~cpu ~debug_state
      | None -> Printf.eprintf "Unknown command %s\n%!" cmd_str)
    | None ->
      Printf.eprintf "Bye!\n";
      exit 0
  done
