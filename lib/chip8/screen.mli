open Stdint

type t

(** Create a new screen buffer with the given dimensions.

@param w the width of the buffer
@param h the height of the buffer
@return the newly created screen buffer *)
val create : w:int -> h:int -> t

(** Return a copy of the screen buffer. Items with a value of [true] will
be rendered, all else will be ignored. *)
val buffer : t -> bool array

(** Clear the screen of any previously written values. *)
val clear : t -> unit

(** Draw to the screen at the given position. The program memory along with the
I register are used to fetch sprites.

@param memory the program memory.
@param i the value of the I register.
@param x the X position to draw to.
@param y the Y position to draw to.
@param rows the number of rows to fetch from I when drawing the sprite.
@return the vF flag for draw, indicating if a collision had occured. *)
val draw : t
  -> memory:Memory.t
  -> i:uint16
  -> x:int
  -> y:int
  -> rows:int
  -> uint8
