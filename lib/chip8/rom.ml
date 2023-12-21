let load path =
  In_channel.with_open_bin path In_channel.input_all
  |> Bytes.of_string
