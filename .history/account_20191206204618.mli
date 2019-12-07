val outacc : Yojson.Basic.t -> 'a -> unit


val account_str : State.t -> string -> 
  [> `Assoc of (string * [> `Int of int | `String of string ]) list ]