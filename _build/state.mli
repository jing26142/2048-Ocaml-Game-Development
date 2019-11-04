

(**
   Representation of dynamic adventure state.

   This module represents the state of the game as it is being played,
   mainly the grid, but also the current score, and functions that cause the
   state to change.
*)

(** The abstract type of values representing the game state. *)
type t

(** [init_state ()] is initial state of the game. In that state the grid will
    have 2 random non-empty cells each containing a box of value 2. *)
val init_state : unit -> t

(** [grid st] is the current grid of the state. *)
val grid : t -> Grid.t

(** [score st] is the current score of the state. *)
val score : t -> int

val new_state: Grid.t -> int -> t