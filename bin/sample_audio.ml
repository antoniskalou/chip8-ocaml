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

  let fill_buffer sampler output =
    let open Bigarray in
    for i = 0 to Array1.dim output - 1 do
      output.{i} <- Int.of_float (128. *. sampler i) + 128;
    done

  let audio_callback time output =
    let open Bigarray in
    let volume = 0.1 in
    let frequency = 200. in
    Printf.printf "Sampling %i bytes...\n%!" (Array1.dim output);
    let sampler _ =
      let x = Float.(2. *. pi *. !time *. frequency) in
      time := !time +. (1. /. (Float.of_int audio_freq));
      volume *. square_wave x
    in
    fill_buffer sampler output

  let audio_thread device_id =
    let open Bigarray in
    let output = Array1.create int8_unsigned c_layout audio_samples in
    let time = ref 0.0 in
    while true do
      if Sdl.get_audio_device_status device_id = Sdl.Audio.playing then begin
        audio_callback time output;
        (match Sdl.queue_audio device_id output with
        | Ok () -> ()
        | Error (`Msg e) -> failwith e)
      end;
      Thread.delay Float.(of_int audio_samples /. of_int audio_freq)
    done

  let create () =
    let audio_spec : Sdl.audio_spec =
      { as_freq = audio_freq
      ; as_format = Sdl.Audio.u8
      ; as_channels = 1
      ; as_samples = audio_samples
      ; as_silence = 0
      (* 1 channel with 1 byte, no need to multiply *)
      ; as_size = Int32.of_int audio_samples
      ; as_callback = None
      }
    in
    Sdl.open_audio_device None false audio_spec 0
    |> Result.map (fun (device_id, _) ->
        let _thread = Domain.spawn (fun () ->
          audio_thread device_id) in
        { device_id })


  let play { device_id; _ } =
    Sdl.pause_audio_device device_id false

  let pause { device_id; _ } =
    Sdl.pause_audio_device device_id true
end

let () =
  Sdl.init Sdl.Init.(video + events + audio) |> or_exit;
  let _ = Sdl.create_window ~w:640 ~h:480 "Audio Test" Sdl.Window.vulkan |> or_exit in
  let audio = Buzzer.create () |> or_exit in
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
