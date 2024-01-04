open Stdint

type t

(** The base address that the ROM is usually stored at. *)
val rom_base_address : uint16

(** Create new memory of 4KB, as defined in the chip8 spec. *)
val create : unit -> t

(** Create memory using {!Stdlib.Bytes}. *)
val of_bytes : Bytes.t -> t

(** Returns the size of the memory buffer in bytes. *)
val size : t -> int

(** Loads bytes into memory at the given position.

@param src The {!Stdlib.Bytes} to load.
@param pos The position at which to load them in. *)
val load : t -> src:Bytes.t -> pos:uint16 -> unit

(** Read a 16-bit value from memory at the given position.

@param pos The position at which to read the memory.
@return a 16-bit integer.
@raise Invalid_argument if the memory location does not exist. *)
val read_uint16 : t -> pos:uint16 -> uint16

(** Read an 8-bit value from memory at the given position.

@param pos The position at which to read the memory.
@return a 8-bit integer.
@raise Invalid_argument if the memory location does not exist. *)
val read_uint8 : t -> pos:uint16 -> uint8

(** Write a 16-bit value into memory at the given position.

@param pos The position at which to write to memory.
@raise Invalid_argument if the memory location does not exist. *)
val write_uint16 : t -> pos:uint16 -> uint16 -> unit

(** Write an 8-bit value into memory at the given position.

@param pos The position at which to write to memory.
@raise Invalid_argument if the memory location does not exist. *)
val write_uint8 : t -> pos:uint16 -> uint8 -> unit
