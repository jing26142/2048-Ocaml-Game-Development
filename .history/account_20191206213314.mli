
type t

val account_str : State.t ->
  string ->
  int -> [> `Assoc of (string * [> `Int of int | `String of string ]) list ]

val account_rep_of_json : Yojson.Basic.t -> t

val games_played : t -> int