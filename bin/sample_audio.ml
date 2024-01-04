open Stdint
open Tsdl
open Chip8

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit;
  let _ = Sdl.create_window ~w:640 ~h:480 "Audio Test" Sdl.Window.vulkan |> or_exit in
  let audio = Buzzer.create { volume = 0.2; frequency = 150. } |> or_exit in
  Buzzer.play audio;
  let e = Sdl.Event.create () in
  let rec loop () =
    Sdl.wait_event (Some e) |> or_exit;
    match Sdl.Event.(enum (get e typ)) with
    | `Key_down ->
      let scancode = Sdl.Event.(get e keyboard_scancode) in
      (match Sdl.Scancode.enum scancode with
       | `Escape -> exit 0
       | _ -> ())
    | `Quit ->
      Sdl.quit ()
    | _ -> loop ()
  in
  loop ()
