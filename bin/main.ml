open Tsdl

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

let draw_graphics renderer =
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
  let renderer = init_graphics () in
  let last_tick = ref 0. in
  while true do
    if (Unix.gettimeofday () -. !last_tick) >= threshold then begin
      clear_graphics renderer;
      draw_graphics renderer;
      handle_event ();
      last_tick := Unix.gettimeofday ()
    end
  done
