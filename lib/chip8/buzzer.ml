open Tsdl

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

type config = { volume : float; frequency : float }

type callback_state =
  { config : config
  ; spec : Sdl.audio_spec
  ; mutable time : float
  }

let audio_callback ~state output =
  let { volume; frequency; _ } = state.config in
  let sampler _ =
    let x = Float.(2. *. pi *. state.time *. frequency) in
    state.time <- state.time +. (1. /. (Float.of_int state.spec.as_freq));
    volume *. square_wave x
  in
  fill_buffer sampler output

let audio_thread device_id ~state =
  let open Bigarray in
  let samples = state.spec.as_samples in
  let freq = state.spec.as_freq in
  let output = Array1.create int8_unsigned c_layout samples in
  while true do
    if Sdl.get_audio_device_status device_id = Sdl.Audio.playing then begin
      audio_callback ~state output;
      (match Sdl.queue_audio device_id output with
        | Ok () -> ()
        | Error (`Msg e) -> failwith e)
    end;
    (* fill the buffer a bit more than needed, it seems to reduce
       crackling/stuttering *)
    let queue_size = Sdl.get_queued_audio_size device_id in
    if queue_size > samples * 2 then
      (* wait for the device to drain, based off SDL_RunAudio *)
      Thread.delay Float.(of_int samples /. of_int freq)
  done

let default_freq = 44100
let default_samples = 512

type t = { device_id : int32 }

let create config =
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
  |> Result.map (fun (device_id, spec) ->
      (* had to make our own audio thread because of a deadlocking issue in the SDL
         audio callback.

        According to this issue https://github.com/dbuenzli/tsdl/issues/13 this
        is fixed, but the issue still seems to exist. *)
      let _thread = Domain.spawn (fun () ->
          let state = { config; spec; time = 0.} in
          audio_thread device_id ~state)
      in
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
