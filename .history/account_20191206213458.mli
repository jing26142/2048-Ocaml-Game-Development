
type t

val account_str : State.t ->
  string ->
  'a ->
  'a -> [> `Assoc of (string * [> `Int of 'a | `String of string ]) list ]

val account_rep_of_json : Yojson.Basic.t -> t

val games_played : t -> int