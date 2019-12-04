(**
   Representation of grid data structure.

   This module represents the data stored in 4x4 grid, including
   the individuals and the boxes.
*)

(** The abstract type of the 4x4 grid *)
type t

(** The type of a box *)
type box

(** The type of an individual cell in the grid. *)
type cell = box option

val row: int -> t list -> t

(** [address a b g] returns the cell of location ([a],[b]) in grid [g], where
    [a] is the row number and [b] is the column number.
    Requires: a, b in 0..3 *)
val address : int -> int -> t -> cell

(** [gen_box v a b g] is a [g] with box of value [v] in cell with
    position (a, b).
    Requires: cell at position (a, b) is empty. *)
val gen_box: int -> int -> int -> t -> t

(** [remove_box a b g] is [g] with cell location (a,b) as None
*)
val remove_box: int -> int -> t -> t

(** [value bx] is the value associated with [bx] *)
val value : box -> int

val pos: box -> int*int

(** [empty ()] is the empty grid. *)
val empty : unit -> t

val grid_size : t -> int

(**[box_of_cell (Some box)] is [box].
   Raise: Failure if not box *)
val box_of_cell: box option -> box

(**[is_empty_cell box] is true if box is empty *)
val is_empty_box: int -> bool

val to_matrix: t -> int list list

val random: t -> t

(** [win g] is true iff it has a cell containing the box with value 2048.
*)
val win : t -> bool

(** [lose g] is true iff none of the cells are empty and no two adjacent
    boxes have the same value. *)
val lose : t -> bool

val content_box : box option -> int


