
type t

val account_str : State.t ->
  string ->
  'a ->
  'a ->
  string ->
  string ->
  string ->
  string -> [> `Assoc of (string * [> `Int of 'a | `String of string ]) list ]

val account_rep_of_json : Yojson.Basic.t -> t

val games_played : t -> int

val name : t -> string

val last_score : t -> int

val date_joined : t -> string

val last_played : t -> string

val all_scores : t -> string

val title : t -> string