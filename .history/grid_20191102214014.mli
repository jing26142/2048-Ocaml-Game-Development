(**
   Representation of grid data structure.

   This module represents the data stored in 4x4 grid, including
   the individuals and the boxes.
*)

(** The abstract type of the 4x4 grid *)
type t




(** The type of a box *)
type box = int

type cell = box option

(** [address a b g] returns the cell of location ([a],[b]) in grid [g] *)
val address : int -> int -> t -> cell

val gen_box: t -> int -> int -> int -> t

val value: box -> box

val empty: t