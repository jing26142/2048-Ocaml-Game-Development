

(**
   Representation of dynamic adventure state.

   This module represents the state of the game as it is being played,
   mainly the grid, but also the current score, and functions that cause the
   state to change.
*)

(** The abstract type of values representing the game state. *)
type t

(**The type of values representing direction *)
type dir = U|D|L|R

val string_row : Grid.box option array -> string

val string_rep : Grid.t -> string

(** [init_state ()] is initial state of the game. In that state the grid will
    have 2 random non-empty cells each containing a box of value 2. *)
val init_state : unit -> t

(** [grid st] is the current grid of the state. *)
val grid : t -> Grid.t

(** [score st] is the current score of the state. *)
val score : t -> int

val gamelog : t -> string

val new_state: Grid.t -> int -> string -> t

val update_score : t -> int -> unit

(**[move_all_down state grid] creates a new tuple of (grid, score) with all boxes
   moved down as commanded by the user*)
val move_all : t -> dir -> t

(** [copy st] is a copy of [st] such that any changes made to [copy g] will 
    leave [st] unaffected and vice versa. *) 
val copy : t -> t 

(**[string_save g] creates a JSON representation of the grid g*)  
val string_save : Grid.t -> [> `Assoc of (string * [> `String of string ]) list ]