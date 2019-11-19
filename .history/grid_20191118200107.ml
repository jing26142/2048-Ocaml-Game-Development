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

let empty () =
  [Array.make 4 None; Array.make 4 None; Array.make 4 None; Array.make 4 None]

let grid_size g =
  Array.length (help_add 0 0 g)

let rec to_matrix g =
  match g with
  |[] -> []
  |h::t -> Array.fold_left (fun lst cell -> lst @ [content_box cell]) [] h ::
           to_matrix t

let is_full g =
  not (List.exists (Array.exists (fun cell -> cell = None)) g)

let random_help = function
  |None -> true
  |Some box -> false

let rec random g =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  if is_full g then g else if (random_help (address r11 r12 g)) then gen_box 2 r11 r12 g
  else random g

let win g =
  List.exists (Array.exists (fun cell -> content_box cell = 2048)) g

let l_hori_hlpr row =
  let acc = ref false in
  let _ = for i = 0 to 2
    do
      acc := !acc || content_box row.(i) = content_box row.(i + 1)
    done in
  !acc

let rec l_vert_hlpr i = function
  | h1 :: (h2 :: t) -> content_box h1.(i) = content_box h2.(i) || l_vert_hlpr i (h2 :: t)
  | h1 :: [] -> false
  | [] -> failwith "bad input"

let l_vert g =
  let acc = ref false in
  let _ = for i = 0 to 3
    do
      acc := !acc || l_vert_hlpr i g
    done in
  !acc

let lose g =
  if List.exists (Array.exists (fun cell -> cell = None)) g then false else
  if List.fold_left (fun acc row -> acc || l_hori_hlpr row) false g then
    false
  else not (l_vert g)







