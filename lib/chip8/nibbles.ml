open Stdint

let to_uint8 (n1: int) (n2: int): uint8 =
  Uint8.of_int ((n1 lsl 4) + n2)

let to_uint16 (n1: int) (n2: int) (n3: int): uint16 =
  Uint16.of_int ((n1 lsl 8) + (n2 lsl 4) + n3)

let of_uint8 (x: uint8) : int * int =
  let x = Uint8.to_int x in
  (
    (0xF0 land x) lsr 4,
    0x0F land x
  )

let of_uint16 (x : uint16) : int * int * int * int =
  let x = Uint16.to_int x in
  (
    (* x >> 12 *)
    x lsr 12,
    (* (0x0F00 & x) >> 8 *)
    (0x0F00 land x) lsr 8,
    (* (0x00F0 & x) >> 4 *)
    (0x00F0 land x) lsr 4,
    (* 0x000F & x *)
    0x000F land x
  )
