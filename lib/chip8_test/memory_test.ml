open Chip8
open Stdint

let%test "create returns 4kb of empty memory" =
  let memory = Memory.create () in
  let size = Memory.size memory in
  size = 4096 && (
    (fun i -> Memory.read_uint8 memory ~pos:(Uint16.of_int i))
    |> Seq.init size
    |> Seq.for_all (fun x -> x = Uint8.zero)
  )

let%test "of_bytes creates memory with the given byte data" =
  let bytes = Bytes.make 2048 'A' in
  let memory = Memory.of_bytes bytes in
  Bytes.length bytes = Memory.size memory &&
    Memory.read_uint8 memory ~pos:Uint16.zero = Uint8.of_int 65

let%test "size returns the size of memory" =
  Memory.size (Memory.create ()) = 4096

let%test "load will load a set of bytes at a position" =
  let memory = Memory.create () in
  let src = Bytes.make 100 'A' in
  Memory.load memory ~src ~pos:(Uint16.of_int 100);
  Memory.read_uint8 memory ~pos:Uint16.zero = Uint8.zero &&
    Memory.read_uint8 memory ~pos:(Uint16.of_int 100) = Uint8.of_int 65

let%test "load will fail when given an invalid position" =
  let memory = Memory.create () in
  let src = Bytes.make 100 'A' in
  try
    Memory.load memory ~src ~pos:(Uint16.max_int);
    false
  with Invalid_argument _ -> true

let%test "load fails if given a src that's too large" =
  let memory = Memory.create () in
  let src = Bytes.make 2048 'A' in
  try
    Memory.load memory ~src ~pos:(Uint16.of_int 4000);
    false
  with Invalid_argument _ -> true

let%test "read_uint16 returns a uint16 at pos" =
  let memory = Memory.create () in
  let src = Bytes.make 100 'A' in
  Memory.load memory ~src ~pos:Uint16.zero;
  (* 0x41 is ASCII character A *)
  Memory.read_uint16 memory ~pos:Uint16.zero = Uint16.of_int 0x4141

let%test "read_uint16 fails when given an invalid pos" =
  let memory = Memory.create () in
  try
    Memory.read_uint16 memory ~pos:Uint16.max_int |> ignore;
    false
  with Invalid_argument _ -> true

let%test "read_uint8 returns a uint8 at pos" =
  let memory = Memory.create () in
  let src = Bytes.make 100 'A' in
  Memory.load memory ~src ~pos:Uint16.zero;
  Memory.read_uint8 memory ~pos:Uint16.zero = Uint8.of_int 0x41

let%test "read_uint8 fails when given an invalid pos" =
  let memory = Memory.create () in
  try
    Memory.read_uint8 memory ~pos:Uint16.max_int |> ignore;
    false
  with Invalid_argument _ -> true

let%test "write_uint16 writes a 16-bit int into memory" =
  let memory = Memory.create () in
  Memory.write_uint16 memory ~pos:Uint16.zero (Uint16.of_int 0xFFFF);
  Memory.read_uint16 memory ~pos:Uint16.zero = Uint16.of_int 0xFFFF &&
    (* check that next memory address wasn't written to *)
    Memory.read_uint16 memory ~pos:(Uint16.of_int 2) = Uint16.zero

let%test "write_uint16 fails when writing to invalid memory" =
  let memory = Memory.create () in
  try
    Memory.write_uint16 memory ~pos:Uint16.max_int (Uint16.of_int 0xFFFF);
    false
  with Invalid_argument _ -> true

let%test "write_uint8 writes an 8-bit int into memory" =
  let memory = Memory.create () in
  Memory.write_uint8 memory ~pos:Uint16.zero (Uint8.of_int 0xFF);
  Memory.read_uint8 memory ~pos:Uint16.zero  = Uint8.of_int 0xFF &&
    Memory.read_uint8 memory ~pos:(Uint16.of_int 1) = Uint8.zero

let%test "write_uint8 fails when writing to invalid memory" =
  let memory = Memory.create () in
  try
    Memory.write_uint8 memory ~pos:Uint16.max_int (Uint8.of_int 0xFF);
    false
  with Invalid_argument _ -> true
