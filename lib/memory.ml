open Stdint

type t = bytes

let rom_base_address = Uint16.of_int 0x200

let create () : t = Bytes.create 4096
let of_bytes b = b

let size (t: t): int = Bytes.length t

let load (t: t) ~src ~pos =
  Bytes.blit src 0 t (Uint16.to_int pos) (Bytes.length src)

let read_uint16 (t: t) ~(pos: uint16): uint16 =
  Uint16.of_bytes_big_endian t (Uint16.to_int pos)

let read_uint8 (t: t) ~(pos: uint16): uint8 =
  Uint8.of_bytes_big_endian t (Uint16.to_int pos)

let write_uint16 (t: t) ~(pos: uint16) (x: uint16): unit =
  Uint16.to_bytes_big_endian x t (Uint16.to_int pos)

let write_uint8 (t: t) ~(pos: uint16) (x: uint8): unit =
  Uint8.to_bytes_big_endian x t (Uint16.to_int pos)
