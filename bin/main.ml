open Tsdl
open Stdint
open Chip8

(* module Emulator = struct
  type config =
    { render_scale : int
    ; bg_color : uint32
    ; fg_color : uint32
    }

  type t =
    { cpu : Cpu.t
    ; memory : Memory.t
    }
end *)

let render_scale = 20
let target_fps = 60
let target_mhz = 540
(* the number of cycles that run before a refresh occurs *)
let cycles_per_refresh = target_mhz / target_fps
let refresh_per_second = (1. /. Float.of_int target_fps)

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let init_sdl2 () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit

let init_graphics () =
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

let scancode_to_key =
  function
  | `K1 -> Some 0x1
  | `K2 -> Some 0x2
  | `K3 -> Some 0x3
  | `K4 -> Some 0xC
  | `Q -> Some 0x4
  | `W -> Some 0x5
  | `E -> Some 0x6
  | `R -> Some 0xD
  | `A -> Some 0x7
  | `S -> Some 0x8
  | `D -> Some 0x9
  | `F -> Some 0xE
  | `Z -> Some 0xA
  | `X -> Some 0x0
  | `C -> Some 0xB
  | `V -> Some 0xF
  | _ -> None

let handle_event cpu =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      (match Sdl.Scancode.enum scancode with
      | `Escape -> exit 0
      | code ->
        scancode_to_key code
        |> Option.iter (fun k -> Cpu.press_key cpu k true))
    | `Key_up ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      Sdl.Scancode.enum scancode
      |> scancode_to_key
      |> Option.iter (fun k-> Cpu.press_key cpu k false)
    | `Quit -> exit 0
    | _ -> ()
  end else ()

let timed f =
  let start = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. start

let seconds_to_ms s = Int32.of_float (s *. 1000.)

let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    Printf.eprintf "Usage: %s <ROM FILE>\n" Sys.executable_name;
    exit 2
  end;
  init_sdl2 ();
  let rom = Rom.load argv.(1) in
  let memory = Memory.create () in
  Memory.load memory ~src:Fonts.fonts ~pos:Uint16.zero;
  Memory.load memory ~src:rom ~pos:Memory.rom_base_address;
  let cpu = Cpu.create memory in
  let buzzer = Buzzer.create ~volume:0.05 ~frequency:200. |> or_exit in
  let renderer = init_graphics () in
  while true do
    handle_event cpu;
    clear_graphics renderer;
    Cpu.update_timers cpu;
    let elapsed =
      timed (fun () ->
        for i = 0 to cycles_per_refresh do
          Cpu.tick cpu
        done)
    in
    if Cpu.sound_playing cpu
    then Buzzer.play buzzer
    else Buzzer.pause buzzer;
    draw_graphics (Cpu.screen_buffer cpu) renderer;
    Sdl.delay (seconds_to_ms (refresh_per_second -. elapsed))
  done
