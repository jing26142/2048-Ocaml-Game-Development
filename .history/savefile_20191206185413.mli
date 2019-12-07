type t

(**[string_save g] creates a JSON representation of the grid g*)  
val string_save :State.t ->
  [> `Assoc of (string * [> `Int of int | `String of string ]) list ]

val state_rep_of_json : Yojson.Basic.t -> t

val create_state : t -> State.t