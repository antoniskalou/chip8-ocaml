open Tsdl
open Stdint

module Nibbles = struct
  let to_uint8 (n1: int) (n2: int): uint8 =
    Uint8.of_int ((n1 lsl 4) + n2)

  let to_uint16 (n1: int) (n2: int) (n3: int): uint16 =
    Uint16.of_int ((n1 lsl 8) + (n2 lsl 4) + n3)

  let of_uint8 (x: uint8) : int * int =
    let x = Uint8.to_int x in
    (
      (0xF0 land x) lsr 4,
      0x0F land x
    )

  let of_uint16 (x : uint16) : int * int * int * int =
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
end

module Rom = struct
  let load path =
    In_channel.with_open_bin path In_channel.input_all
    |> Bytes.of_string
end

module Memory = struct
  type t = bytes

  let rom_base_address = Uint16.of_int 0x200

  let create () : t = Bytes.create 4096
  (* let of_bytes b = b *)

  let load (t: t) ~src ~pos =
    Bytes.blit src 0 t (Uint16.to_int pos) (Bytes.length src)

  let read_uint16 (t: t) ~(pos: uint16): uint16 =
    pos
    |> Uint16.to_int
    |> Bytes.get_uint16_be t
    |> Uint16.of_int

  let read_uint8 (t: t) ~(pos: uint16): uint8 =
    pos
    |> Uint16.to_int
    |> Bytes.get_uint8 t
    |> Uint8.of_int

  let write_uint16 (t: t) ~(pos: uint16) (x: uint16): unit =
    Bytes.set_uint16_be t (Uint16.to_int pos) (Uint16.to_int x)

  let write_uint8 (t: t) ~(pos: uint16) (x: uint8): unit =
    Bytes.set_uint8 t (Uint16.to_int pos) (Uint8.to_int x)
end

module Cpu = struct
  type register = uint8

  type t =
    { registers : register array
    ; mutable i : uint16
    ; mutable pc : uint16
    ; mutable sp : uint16
    (* represents a render buffer, when true is encountered a pixel
       is drawn at that coordinate *)
    ; screen : bool array
    }

  let create () =
    { registers = Array.make 16 Uint8.zero
    ; i = Uint16.zero
    ; pc = Memory.rom_base_address
    (* caml8 uses this address, though we can use whatever stack-like
       structure we prefer. *)
    ; sp = Uint16.of_int 0xFA0
    ; screen = Array.make (64 * 32) false
    }

  let screen_buffer t = t.screen

  let fetch (t: t) memory =
    let opcode = Memory.read_uint16 memory ~pos:t.pc in
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
    | (0xA, n1, n2, n3) -> Set_index (Nibbles.to_uint16 n1 n2 n3)
    | (0xB, n1, n2, n3) -> Jump0 (Nibbles.to_uint16 n1 n2 n3)
    | (0xD, x, y, n) -> Draw (Uint8.of_int x, Uint8.of_int y, Uint8.of_int n)
    | _ ->
      let opcode_str = Printf.sprintf "0x%04X" (Uint16.to_int opcode) in
      raise (Unknown_opcode (opcode_str, opcode))

  let execute t memory instruction =
    match instruction with
    | Clear -> Array.fill t.screen 0 (Array.length t.screen) false
    | Set (vx, x) ->
      t.registers.(Uint8.to_int vx) <- x
    | Set_index (i) ->
      t.i <- i
    | Draw (vx, vy, rows) ->
      t.registers.(0xF) <- Uint8.zero;
      for y = 0 to Uint8.to_int rows - 1 do
        let line =
          Memory.read_uint8 memory ~pos:Uint16.(t.i + of_int y)
          |> Uint8.to_int
        in
        for x = 0 to 7 do
          let bit = (line lsr (7 - x)) land (0b00000001) = 1 in
          let screen_idx = ((y + Uint8.to_int vy) mod 32) * 64 + (x + (Uint8.to_int vx mod 64)) in
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

  let tick t memory =
    fetch t memory
    |> decode
    |> execute t memory
end

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let init_graphics () =
  Sdl.init Sdl.Init.(video + events) |> or_exit;
  let w = Sdl.create_window ~w:640 ~h:320 "Chip8 (Vulkan)" Sdl.Window.vulkan |> or_exit in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer

let clear_graphics renderer =
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let draw_graphics buffer renderer =
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  buffer |> Array.iteri (fun i b ->
    if b then begin
      (* there are probably more efficient ways to do this rather
         than creating a rect per pixel*)
      let x = 10 * (i mod 64) in
      let y = 10 * (i / 64) in
      let rect = Sdl.Rect.create ~x ~y ~w:10 ~h:10 in
      Sdl.render_fill_rect renderer (Some rect) |> or_exit
    end);
  Sdl.render_present renderer

let handle_event () =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Quit -> exit 0
    | _ -> ()
  end

let threshold = 1. /. 360.

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  let rom = Rom.load argv.(1) in
  let memory = Memory.create () in
  Memory.load memory ~src:rom ~pos:Memory.rom_base_address;
  let cpu = Cpu.create () in
  let renderer = init_graphics () in
  let last_tick = ref 0. in
  while true do
    if (Unix.gettimeofday () -. !last_tick) >= threshold then begin
      Cpu.tick cpu memory;
      clear_graphics renderer;
      draw_graphics (Cpu.screen_buffer cpu) renderer;
      handle_event ();
      last_tick := Unix.gettimeofday ()
    end
  done
