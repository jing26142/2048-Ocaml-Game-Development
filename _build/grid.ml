exception Failure

type box = {v: int; pos: int*int}

type cell = box option

type t = cell array list


let rec help_add i count g =
  match g with
  | h :: t -> if (i = count) then h else help_add i (count+1) t
  | _ -> raise Failure

(**[address a b c] returns the cell of location ([a],[b]) in grid [c]*)
let address a b g =
  (help_add a 0 g).(b)

let box_of_cell = function
  |Some box -> box
  |None -> raise Failure



(**a is row, b is col *)  
let gen_box (v:int) a b g =
  (help_add a 0 g).(b) <- Some {v = v; pos = (a,b)}; g

let remove_box a b g =
  (help_add a 0 g).(b) <- None; g

let value box = box.v

let content_box = function
  |None -> 0
  |Some box -> value box

let pos box = box.pos

let empty =
  [Array.make 4 None; Array.make 4 None; Array.make 4 None; Array.make 4 None]

let grid_size g =
  Array.length (help_add 0 0 g)

let rec to_matrix g =
  match g with
  |[] -> []
  |h::t -> Array.fold_left (fun lst cell -> lst @ [content_box cell]) [] h ::
           to_matrix t

