(* TODO: grid data structure structure *)
exception Failure
type t = {
  value : int;
  pos : int*int;
}

type cell = {
  content : t option;
}

type gridd = {
  grid : cell array list;
}

let rec help_add i count g= 
  match g with
  |h::t -> if (i = count) then h else help_add i (count+1) t
  |_ -> raise Failure


(**[address a b c] returns the cell of location ([a],[b]) in grid [c]*)
let address a b g = 
  (help_add a 0 g).(b)
