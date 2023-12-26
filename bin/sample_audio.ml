open Stdint
open Tsdl

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let audio_freq = 8000
let audio_samples = 512

let time = ref 0.0

let audio_callback output =
  let open Bigarray in
  let volume = 0.2 in
  let frequency = 200. in
  Printf.printf "Sampling %i bytes...\n" (Array1.dim output);
  (* number of samples * channels *)
  for i = 0 to Array1.dim output - 1 do
    let x = Float.(2. *. pi *. !time *. frequency) in
    output.{i} <- Int.of_float (128. *. volume *. sin x) + 128;
    Printf.printf "x = %f, sin(x) = %f, output = %i\n%!" x (sin x) output.{i};
    time := !time +. (1. /. (Float.of_int audio_freq));
  done

let audio_spec : Sdl.audio_spec =
  { as_freq = audio_freq
  ; as_format = Sdl.Audio.u8
  ; as_channels = 1
  ; as_samples = audio_samples
  ; as_silence = 0
  (* 1 channel with 1 byte, no need to multiply *)
  ; as_size = Int32.of_int audio_samples
  ; as_callback =
      Some (Sdl.audio_callback Bigarray.int8_unsigned audio_callback)
  }

let () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit;
  let window = Sdl.create_window ~w:640 ~h:480 "Audio Test" Sdl.Window.vulkan |> or_exit in
  let (device, _) = Sdl.open_audio_device None false audio_spec 0 |> or_exit in
  Sdl.pause_audio_device device false;
  let e = Sdl.Event.create () in
  let rec loop () =
    Sdl.wait_event (Some e) |> or_exit;
    match Sdl.Event.(enum (get e typ)) with
    | `Quit ->
      Sdl.pause_audio_device device true;
      Sdl.destroy_window window;
      Sdl.quit ()
    | _ -> loop ()
  in
  loop ()
