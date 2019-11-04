(**
   Representation of grid data structure.

   This module represents the data stored in 4x4 grid, including
   the individuals and the boxes.
*)

(** The abstract type of the 4x4 grid *)
type t

(** The type of a box *)
type box = int

(** The type of an individual cell in the grid. *)
type cell = box option

(** [address a b g] returns the cell of location ([a],[b]) in grid [g], where
    [a] is the row number and [b] is the column number.
    Requires: a, b in 0..3 *)
val address : int -> int -> t -> cell

(** [gen_box v a b g] is a copy of [g] with box of value [v] in cell with
    position (a, b).
    Requires: cell at position (a, b) is empty. *)
val gen_box: int -> int -> int -> t -> t

(** [value bx] is the value associated with [bx] *)
val value : box -> int

(** [empty] is the empty grid. *)
val empty : t