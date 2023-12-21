open Chip8
open Stdint

let%test "to_uint8" = Uint8.of_int 0xAB = Nibbles.to_uint8 0xA 0xB
let%test "to_uint16" =
  Uint16.of_int 0xABC = Nibbles.to_uint16 0xA 0xB 0xC

let%test "of_uint8" =
  let (n1, n2) = Nibbles.of_uint8 (Uint8.of_int 0xAB) in
  n1 = 0xA && n2 = 0xB

let%test "of_uint16" =
  let (n1, n2, n3, n4) = Nibbles.of_uint16 (Uint16.of_int 0xABCD) in
  n1 = 0xA && n2 = 0xB && n3 = 0xC && n4 = 0xD
