open Tsdl
open Tsdl_mixer

let or_exit =
  function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let play_until_done audio =
  let _ = Mixer.play_channel (-1) audio 0 |> or_exit in
  while true do () done

let () =
  let file = Sys.argv.(1) in
  Printf.eprintf "Initializing SDL2.\n";
  (* Sdl.init Sdl.Init.audio |> or_exit; *)
  Printf.eprintf "Initializing mixer.\n";
  Mixer.(init Init.empty) |> or_exit |> ignore;
  Mixer.open_audio
    Mixer.default_frequency
    Mixer.default_format
    Mixer.default_channels
    4096
    |> or_exit;
  Printf.eprintf "Loading file %s.\n" file;
  let audio = Mixer.load_wav file |> or_exit in
  Printf.eprintf "Loaded audio OK.\n";
  (* let channel = Mixer.play_channel (-1) audio 0 |> or_exit in *)
  Printf.eprintf "Playing audio...\n";
  play_until_done audio
