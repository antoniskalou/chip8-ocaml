open Tsdl
open Stdint
open Chip8

let target_fps = 60
let target_mhz = 540
(* the number of cycles that run before a refresh occurs *)
let cycles_per_refresh = target_mhz / target_fps
let refresh_per_second = (1. /. Float.of_int target_fps)

type config =
  { render_scale : int (* TODO: make explicit width *)
  ; bg_color : uint32
  ; fg_color : uint32
  }

let rgba_of_uint32 i =
  let i = Uint32.to_int i in
  (
    (i lsr 16) land 0xFF, (* r *)
    (i lsr 8) land 0xFF,  (* g *)
    i land 0xFF,          (* b *)
    0xFF
  )

let or_exit =
  function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let init_sdl2 () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit

let init_graphics config =
  let w =
    Sdl.create_window
      ~w:(64 * config.render_scale) ~h:(32 * config.render_scale)
      "Chip8 (Vulkan)" Sdl.Window.vulkan
    |> or_exit
  in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  (* render to white first. we know if its visible the screen was never cleared or
    rendered to. *)
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer

let clear_graphics { bg_color; _ } renderer =
  let (r, g, b, a) = rgba_of_uint32 bg_color in
  Sdl.set_render_draw_color renderer r g b a |> or_exit;
  Sdl.render_clear renderer |> or_exit

let draw_graphics { fg_color; render_scale; _ } buffer renderer =
  let (r, g, b, a) = rgba_of_uint32 fg_color in
  Sdl.set_render_draw_color renderer r g b a |> or_exit;
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

let seconds_to_ms s = Int32.of_float (s *. 1000.)

let print_license () =
  print_endline {|
    Chip8 virtual machine written in OCaml
    Copyright (C) 2024 Antonis Kalou

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
  |}

let usage_msg = "chip8 <ROM FILE>"

let rom_file = ref ""
let fg_color = ref "0x00FF00"
let bg_color = ref "0x000000"
let render_scale = ref 20

let anon_fun path = rom_file := path

let speclist =
  [("-fg", Arg.Set_string fg_color, "Set the foreground render colour (uses 0x format)");
   ("-bg", Arg.Set_string bg_color, "Set the background render colour (uses 0x format)");
   ("-scale", Arg.Set_int render_scale, "Set the render scale, defaults to 64x32 (original resolution)")]

let bad_arg msg =
  Printf.eprintf "ERROR: %s\n\n" msg;
  Arg.usage speclist usage_msg;
  exit 2

let parse_args () =
  Arg.parse speclist anon_fun usage_msg;
  (* validate rom file param *)
  if String.equal String.empty !rom_file then
    bad_arg "ROM file not provided!";
  (* validate scale param *)
  if !render_scale <= 0 then
    bad_arg "invalid render scale, must be > 0";
  try
    { render_scale = !render_scale
    ; bg_color = Uint32.of_string !bg_color
    ; fg_color = Uint32.of_string !fg_color
    }
  with Invalid_argument _ ->
    bad_arg "Invalid colour provided, must be in the 0x format"

let () =
  let config = parse_args () in
  print_license ();
  init_sdl2 ();
  let rom = Rom.load !rom_file in
  let memory = Memory.create () in
  Memory.load memory ~src:Fonts.fonts ~pos:Uint16.zero;
  Memory.load memory ~src:rom ~pos:Memory.rom_base_address;
  let cpu = Cpu.create memory in
  let buzzer = Buzzer.create { volume = 0.01; frequency = 150. } |> or_exit in
  let renderer = init_graphics config in
  while true do
    handle_event cpu;
    clear_graphics config renderer;
    Cpu.update_timers cpu;
    let elapsed =
      Util.timed (fun () ->
        for _ = 0 to cycles_per_refresh do
          Cpu.tick cpu
        done)
    in
    if Cpu.sound_playing cpu
    then Buzzer.play buzzer
    else Buzzer.pause buzzer;
    draw_graphics config (Cpu.screen_buffer cpu) renderer;
    Sdl.delay (seconds_to_ms (refresh_per_second -. elapsed))
  done
