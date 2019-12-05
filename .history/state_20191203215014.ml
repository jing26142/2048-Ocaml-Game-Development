open Grid
type t = {
  current_grid : Grid.t;
  mutable score : int;
  gamelog : string
}

let init_state () =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  let r21 = Random.int 4 in
  let r22 = Random.int 4 in
  let r21 = if (r21 = r11 && r22 = r22) then (r11+1) mod 4 else r21 in
  let grid = empty() in {
    current_grid = gen_box 2 r11 r12 grid |> gen_box 2 r21 r22;
    score = 0
  }

let grid st =
  st.current_grid

let score st =
  st.score

let new_state  new_grid score =
  {current_grid = new_grid; score = score}


let update_score st new_score =
  st.score <- new_score


let rec move_down state box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if ( r = (grid_size grid) -1) then  (grid, score state)
  else match (address (r+1) c grid) with
    |None -> gen_box (vbox) (r+1) c grid |>
             remove_box r c |> 
             move_down state (box_of_cell (address (r+1) c grid))
    |Some box2 -> if ((value box2) = value box) then
        let new_v = 2*value box in
        update_score state ((score state) + new_v);
        let b1 = remove_box r c grid in
        let b2 = remove_box (r+1) c b1 in
        let b3 = gen_box new_v (r+1) c b2 in
        (b3, score state) 
      else (grid, score state)

let rec down_box state cell (grid, scr) = 
  match cell with 
  |None -> (grid, score state)
  |Some box -> move_down state box grid 

let move_all_down state grid =
  let scr = score state in
  down_box state (address 3 0 grid) (grid, scr) |> down_box state (address 2 0 grid)
  |> down_box state (address 1 0 grid) |> down_box state (address 0 0 grid) 
  |> down_box state (address 3 1 grid) |> down_box state (address 2 1 grid)
  |> down_box state (address 1 1 grid) |> down_box state (address 0 1 grid) 
  |> down_box state (address 3 2 grid) |> down_box state (address 2 2 grid) 
  |> down_box state (address 1 2 grid) |> down_box state (address 0 2 grid)
  |> down_box state (address 3 3 grid) |> down_box state (address 2 3 grid)
  |> down_box state (address 1 3 grid) |> down_box state (address 0 3 grid)



let rec move_right state box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if (c = (grid_size grid) -1) then (grid, score state)
  else (match (address r (c+1) grid) with
      |None -> gen_box (vbox) r (c+1) grid |>
               remove_box r c |> 
               move_right state (box_of_cell (address r (c+1) grid))
      |Some box2 -> if ((value box2) = value box) then
          let new_v = 2*value box in 
          update_score state ((score state) + new_v);
          let grid1 = remove_box r c grid in
          let grid2 = remove_box r (c+1) grid1 in
          let grid3 =  gen_box new_v r (c+1) grid2 in
          (grid3, score state)
        else (grid, score state))



let rec right_box state cell (grid, scr) =
  match cell with
  |None -> (grid, score state)
  |Some box -> move_right state box grid

let move_all_right state grid = 
  let scr = score state in
  right_box state (address 0 3 grid) (grid, scr) |> right_box state (address 0 2 grid)
  |> right_box state (address 0 1 grid) |> right_box state (address 0 0 grid)
  |> right_box state (address 1 3 grid) |> right_box state (address 1 2 grid)
  |> right_box state (address 1 1 grid) |> right_box state (address 1 0 grid)
  |> right_box state (address 2 3 grid) |> right_box state (address 2 2 grid)
  |> right_box state (address 2 1 grid) |> right_box state (address 2 0 grid)
  |> right_box state (address 3 3 grid) |> right_box state (address 3 2 grid)
  |> right_box state (address 3 1 grid) |> right_box state (address 3 0 grid)

let rec move_left state box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if (c = 0) then (grid, score state)
  else (match (address r (c-1) grid) with
      |None -> gen_box (vbox) r (c-1) grid |>
               remove_box r c |> 
               move_left state (box_of_cell (address r (c-1) grid))
      |Some box2 -> if ((value box2) = value box) then
          let new_v = 2*value box in 
          update_score state ((score state) + new_v);
          let grid1 = remove_box r c grid in
          let grid2 = remove_box r (c-1) grid1 in
          let grid3 = gen_box new_v r (c-1) grid2 in
          (grid3, score state)
        else (grid, score state))

let rec left_box state cell (grid, scr) =
  match cell with
  |None -> (grid, score state)
  |Some box -> move_left state box grid

let move_all_left state grid = 
  let scr = score state in
  left_box state (address 0 0 grid) (grid,scr) |> left_box state (address 0 1 grid)
  |> left_box state (address 0 2 grid) |> left_box state (address 0 3 grid)
  |> left_box state (address 1 0 grid) |> left_box state (address 1 1 grid)
  |> left_box state (address 1 2 grid) |> left_box state (address 1 3 grid)
  |> left_box state (address 2 0 grid) |> left_box state (address 2 1 grid)
  |> left_box state (address 2 2 grid) |> left_box state (address 2 3 grid)
  |> left_box state (address 3 0 grid) |> left_box state (address 3 1 grid)
  |> left_box state (address 3 2 grid) |> left_box state (address 3 3 grid)

let rec move_up state box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if (r = 0) then (grid, score state)
  else match address (r - 1) c grid with
    | None -> gen_box (vbox) (r - 1) c grid |> remove_box r c |> 
              move_up state (box_of_cell (address (r - 1) c grid))
    | Some box2 -> if value box2 = value box then
        let new_v = 2 * value box in
        update_score state ((score state) + new_v);
        let grid1 = remove_box r c grid in
        let grid2 = remove_box (r - 1) c grid1 in
        let grid3 = gen_box new_v (r - 1) c grid2 in
        (grid3, score state)
      else (grid, score state)

let rec up_box state cell (grid, scr) =
  match cell with
  | None -> (grid, score state)
  | Some box -> move_up state box grid

let move_all_up state grid = 
  let scr = score state in
  up_box state (address 0 3 grid) (grid, scr) |> up_box state (address 0 2 grid)
  |> up_box state (address 0 1 grid) |> up_box state (address 0 0 grid)
  |> up_box state (address 1 3 grid) |> up_box state (address 1 2 grid)
  |> up_box state (address 1 1 grid) |> up_box state (address 1 0 grid)
  |> up_box state (address 2 3 grid) |> up_box state (address 2 2 grid)
  |> up_box state (address 2 1 grid) |> up_box state (address 2 0 grid)
  |> up_box state (address 3 3 grid) |> up_box state (address 3 2 grid)
  |> up_box state (address 3 1 grid) |> up_box state (address 3 0 grid)

let copy st = {
  current_grid = Grid.copy st.current_grid;
  score = st.score
}