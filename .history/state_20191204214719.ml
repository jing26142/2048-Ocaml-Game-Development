open Grid
type t = {
  current_grid : Grid.t;
  mutable score : int;
  gamelog : string
}

let rec string_row row =
  string_of_int(content_box row.(0))^ "|" ^string_of_int(content_box row.(1))^"|"^
  string_of_int(content_box row.(2))^ "|" ^string_of_int(content_box row.(3))^"|"


let string_rep grid =
  "----------------------------"^
  "\n|" ^ string_row (row 0 grid) ^
  "\n|" ^ string_row (row 1 grid) ^
  "\n|" ^ string_row (row 2 grid) ^
  "\n|" ^ string_row (row 3 grid)


let init_state () =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  let r21 = Random.int 4 in
  let r22 = Random.int 4 in
  let r21 = if (r21 = r11 && r22 = r22) then (r11+1) mod 4 else r21 in
  let grid = empty() in 
  let c_grid = gen_box 2 r11 r12 grid |> gen_box 2 r21 r22 in {
    current_grid = c_grid;
    score = 0;
    gamelog = string_rep (c_grid);
  }

let grid st =
  st.current_grid

let gamelog st =
  st.gamelog

let score st =
  st.score

let new_state  new_grid score gamelog =
  {current_grid = new_grid; score = score; gamelog = gamelog}


let update_score st new_score =
  st.score <- new_score


let delta_score st dir = 
  let clone = State.copy st in 
  let sim = move_all clone dir in 
  (score sim) - (score st)

let delta_score2 st dir =
  let open List in
  let clone = State.copy st in 
  let sim = move_all clone dir in
  let delta1 = (score sim - score st) in
  let dirs = [U; D; L; R] in 
  let deltas = map (fun x -> delta_score sim x) dirs in 
  let dict = combine deltas dirs in 
  let (max :: max2 :: _) = deltas |> sort compare |> rev in 
  delta1 + (max + max2) / 2

(** TODO: DOCUMENT. *)
let cpu_p1 st d = 
  let open List in
  let dirs = [U; D; L; R] in
  let f = 
    if d = 1 then delta_score 
    else if d = 2 then delta_score2 
    else failwith "Invalid difficulty"
  in
  let deltas = map (fun x -> f st x) dirs in 
  let dict = combine deltas dirs in
  let (max :: _) = deltas |> sort compare |> rev in  
  let best_dir = assoc max dict in 
  move_all st best_dir
let copy st = {
  current_grid = Grid.copy st.current_grid;
  score = st.score;
  gamelog = st.gamelog
}