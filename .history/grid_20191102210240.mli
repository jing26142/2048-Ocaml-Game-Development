(**
   Representation of grid data structure.

   This module represents the data stored in 4x4 grid, including
   the individuals and the boxes.
*)

(** The abstract type of the 4x4 grid *)
type t

(** The type of an individual cell in the grid. *)
type cell

(** The type of a box *)
type box

(** [address a b g] returns the cell of location ([a],[b]) in grid [g] *)
val address : int -> int -> t -> cell

val create_box: int -> int -> int -> box