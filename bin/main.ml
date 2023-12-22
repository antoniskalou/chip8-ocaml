open Tsdl
open Stdint
open Chip8

module Emulator = struct
  type config =
    { render_scale : int
    ; bg_color : uint32
    ; fg_color : uint32
    }

  type t =
    { cpu : Cpu.t
    ; memory : Memory.t
    }
end

(* the threshold before a CPU tick happens. It is calculated as
   1 / desired FPS *)
let threshold = 1. /. 400.
let render_scale = 20

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let init_graphics () =
  Sdl.init Sdl.Init.(video + events) |> or_exit;
  let w =
    Sdl.create_window
      ~w:(64 * render_scale) ~h:(32 * render_scale)
      "Chip8 (Vulkan)" Sdl.Window.vulkan
    |> or_exit
  in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer

let clear_graphics renderer =
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let draw_graphics buffer renderer =
  Sdl.set_render_draw_color renderer 0x00 0xFF 0x00 0xFF |> or_exit;
  buffer |> Array.iteri (fun i b ->
    if b then begin
      (* there are probably more efficient ways to do this rather
         than creating a rect per pixel*)
      let x = render_scale * (i mod 64) in
      let y = render_scale * (i / 64) in
      let rect = Sdl.Rect.create ~x ~y ~w:render_scale ~h:render_scale in
      Sdl.render_fill_rect renderer (Some rect) |> or_exit
    end);
  Sdl.render_present renderer

let handle_event () =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      (match Sdl.Scancode.enum scancode with
      | `Escape -> exit 0
      | _ -> ())
    | `Quit -> exit 0
    | _ -> ()
  end

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
  let renderer = init_graphics () in
  let last_tick = ref 0. in
  while true do
    if (Unix.gettimeofday () -. !last_tick) >= threshold then begin
      Cpu.tick cpu;
      clear_graphics renderer;
      draw_graphics (Cpu.screen_buffer cpu) renderer;
      last_tick := Unix.gettimeofday ()
    end;
    (* handle events outside of timed loop as to not miss any events that
       may happen while the timed cycle is waiting *)
    handle_event ()
  done
