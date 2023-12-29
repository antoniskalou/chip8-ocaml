open Tsdl

let default_freq = 8000
let default_samples = 512

let square_wave angle =
  let tau = Float.pi *. 2. in
  if (mod_float angle tau) < Float.pi
  then 1.0
  else -1.0

let fill_buffer sampler output =
  let open Bigarray in
  for i = 0 to Array1.dim output - 1 do
    output.{i} <- Int.of_float (128. *. sampler ()) + 128;
  done

type callback_state =
  { volume : float
  ; frequency : float
  ; mutable time : float
  }

let audio_callback state output =
  let { volume; frequency; _ } = state in
  output |> fill_buffer (fun () ->
      let x = Float.(2. *. pi *. state.time *. frequency) in
      state.time <- state.time +. (1. /. (Float.of_int default_freq));
      volume *. square_wave x)

let audio_thread device_id =
  let open Bigarray in
  let state = { volume = 0.1; frequency = 200.; time = 0. } in
  let output = Array1.create int8_unsigned c_layout default_samples in
  while true do
    if Sdl.get_audio_device_status device_id = Sdl.Audio.playing then begin
      audio_callback state output;
      (match Sdl.queue_audio device_id output with
        | Ok () -> ()
        | Error (`Msg e) -> failwith e);
    end;
    (* wait for the device to drain, based off SDL_RunAudio *)
    Thread.delay Float.(of_int default_samples /. of_int default_freq)
  done

type t =
  { device_id : int32 }

let create ~volume ~frequency =
  let _state = { volume; frequency; time = 0.} in
  let audio_spec : Sdl.audio_spec =
    { as_freq = default_freq
    ; as_format = Sdl.Audio.u8
    ; as_channels = 1
    ; as_samples = default_samples
    ; as_silence = 0
    (* 1 channel with 1 byte, no need to multiply *)
    ; as_size = Int32.of_int default_samples
    ; as_callback = None
    }
  in
  Sdl.open_audio_device None false audio_spec 0
  |> Result.map (fun (device_id, _) ->
      let _thread = Domain.spawn (fun () -> audio_thread device_id) in
      { device_id })

let play { device_id; _ } =
  Sdl.pause_audio_device device_id false

let pause { device_id; _ } =
  Sdl.pause_audio_device device_id true

(* let stop { device_id; state } = *)
(*   Sdl.pause_audio_device device_id true; *)
(*   Sdl.lock_audio_device device_id; *)
(*   state.time <- 0.; *)
(*   Sdl.unlock_audio_device device_id *)
