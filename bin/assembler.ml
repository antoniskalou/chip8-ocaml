open Stdint

let opcode_of_string s =
  s
  |> String.trim
  |> (^) "0x"
  |> Uint16.of_string

let with_pipe ~src ~dest f =
  Out_channel.with_open_bin dest (fun o ->
    In_channel.with_open_text src (fun i ->
      f i o))

let copy_data ~src ~dest f =
  let rec read_line i o =
    match In_channel.input_line i with
    | Some line ->
      Out_channel.output_byte o (f line);
      read_line i o
    | None -> ()
  in
  with_pipe ~src ~dest read_line

let () =
  let argv = Sys.argv in
  if Array.length argv < 3 then begin
    Printf.eprintf "Usage: %s <SOURCE FILE> <OUTPUT>\n" Sys.executable_name;
    exit 2
  end;
  let src = argv.(1) in
  let dest = argv.(2) in
  copy_data ~src ~dest (fun line ->
    String.split_on_char ':' line
    |> (fun l -> List.nth l 1)
    |> opcode_of_string
    |> Uint16.to_int)
