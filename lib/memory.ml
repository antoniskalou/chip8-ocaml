open Stdint

type t = bytes

let rom_base_address = Uint16.of_int 0x200

let create () : t = Bytes.create 4096
let of_bytes b = b

let load (t: t) ~src ~pos =
  Bytes.blit src 0 t (Uint16.to_int pos) (Bytes.length src)

let read_uint16 (t: t) ~(pos: uint16): uint16 =
  pos
  |> Uint16.to_int
  |> Bytes.get_uint16_be t
  |> Uint16.of_int

let read_uint8 (t: t) ~(pos: uint16): uint8 =
  pos
  |> Uint16.to_int
  |> Bytes.get_uint8 t
  |> Uint8.of_int

let write_uint16 (t: t) ~(pos: uint16) (x: uint16): unit =
  Bytes.set_uint16_be t (Uint16.to_int pos) (Uint16.to_int x)

let write_uint8 (t: t) ~(pos: uint16) (x: uint8): unit =
  Bytes.set_uint8 t (Uint16.to_int pos) (Uint8.to_int x)
