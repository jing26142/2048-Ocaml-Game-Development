(**[string_save g] creates a JSON representation of the grid g*)  
val string_save : t -> [> `Assoc of (string * [> `Int of int | `String of string ]) list ]