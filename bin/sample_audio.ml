open Stdint
open Tsdl

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

module Buzzer = struct
  let audio_freq = 8000
  let audio_samples = 512

  type config =
    { volume : float
    ; frequency : float
    }

  type t = { device_id : int32 }

  let square_wave angle =
    let tau = Float.pi *. 2. in
    if (mod_float angle tau) < Float.pi
    then 1.0
    else -1.0

  let audio_callback ~config ~time output =
    let open Bigarray in
    Printf.printf "Sampling %i bytes...\n" (Array1.dim output);
    (* number of samples * channels *)
    for i = 0 to Array1.dim output - 1 do
      let x = Float.(2. *. pi *. !time *. config.frequency) in
      output.{i} <- Int.of_float (128. *. config.volume *. square_wave x) + 128;
      Printf.printf "x = %f, sin(x) = %f, output = %i\n%!" x (sin x) output.{i};
      time := !time +. (1. /. (Float.of_int audio_freq));
    done

  let create config =
    let time = ref 0.0 in
    let audio_spec : Sdl.audio_spec =
      { as_freq = audio_freq
      ; as_format = Sdl.Audio.u8
      ; as_channels = 1
      ; as_samples = audio_samples
      ; as_silence = 0
      (* 1 channel with 1 byte, no need to multiply *)
      ; as_size = Int32.of_int audio_samples
      ; as_callback =
          Some (Sdl.audio_callback Bigarray.int8_unsigned
                  (audio_callback ~config ~time))
      }
    in
    Sdl.open_audio_device None false audio_spec 0
    |> Result.map (fun (device_id, _) -> { device_id })


  let play { device_id; _ } =
    Sdl.pause_audio_device device_id false

  let pause { device_id; _ } =
    Sdl.pause_audio_device device_id true
end

let () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit;
  let window = Sdl.create_window ~w:640 ~h:480 "Audio Test" Sdl.Window.vulkan |> or_exit in
  let audio = Buzzer.create { volume = 0.01; frequency = 200. } |> or_exit in
  Buzzer.play audio;
  let e = Sdl.Event.create () in
  let rec loop () =
    Sdl.wait_event (Some e) |> or_exit;
    match Sdl.Event.(enum (get e typ)) with
    | `Quit ->
      Buzzer.pause audio;
      (* Sdl.close_audio_device device; *)
      Sdl.destroy_window window;
      Sdl.quit ()
    | _ -> loop ()
  in
  loop ()
