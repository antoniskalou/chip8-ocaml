open Tsdl

let default_freq = 8000
let default_samples = 512

let square_wave angle =
  let tau = Float.pi *. 2. in
  if (mod_float angle tau) < Float.pi
  then 1.0
  else -1.0

type callback_state =
  { volume : float
  ; frequency : float
  ; mutable time : float
  }

let audio_callback state output =
  let open Bigarray in
  let { volume; frequency; _ } = state in
  (* number of samples * channels *)
  for i = 0 to Array1.dim output - 1 do
    let x = Float.(2. *. pi *. state.time *. frequency) in
    output.{i} <- Int.of_float (128. *. volume *. square_wave x) + 128;
    state.time <- state.time +. (1. /. (Float.of_int default_freq));
  done

type t = { device_id : int32 }

let create ~volume ~frequency =
  let state = { volume; frequency; time = 0.} in
  let audio_spec : Sdl.audio_spec =
    { as_freq = default_freq
    ; as_format = Sdl.Audio.u8
    ; as_channels = 1
    ; as_samples = default_samples
    ; as_silence = 0
    (* 1 channel with 1 byte, no need to multiply *)
    ; as_size = Int32.of_int default_samples
    ; as_callback =
        Some (Sdl.audio_callback Bigarray.int8_unsigned
                (audio_callback state))
    }
  in
  Sdl.open_audio_device None false audio_spec 0
  |> Result.map (fun (device_id, _) -> { device_id })


let play { device_id; _ } =
  let status = Sdl.get_audio_device_status device_id in
  if status <> Sdl.Audio.playing then begin
    Sdl.pause_audio_device device_id false
  end

let pause { device_id; _ } =
  let status = Sdl.get_audio_device_status device_id in
  if status = Sdl.Audio.playing then begin
    Sdl.pause_audio_device device_id true
  end
