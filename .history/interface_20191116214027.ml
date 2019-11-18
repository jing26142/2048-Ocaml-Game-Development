open State
open Grid
open Command

(*[new_value grid] adds either a 2 or 4 to a random cell containing a zero *)
let rec new_value grid =
  failwith "unimplemented"


(*[display grid] prints a 4 by 4 grid with values of the current state *)
let display grid =
  List.iter (fun line ->
      print_endline "----------------------------";
      print_string " |";
      line
      |> List.map (Printf.sprintf "%4d")
      |> String.concat " | "
      |> print_string;
      print_endline "|"

    ) grid



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

let rec move_down state box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if ( r = (grid_size grid) -1) then ((print_endline "move_down base"); (grid, score state))
  else match (address (r+1) c grid) with
    |None -> gen_box (vbox) (r+1) c grid |>
             remove_box r c |> 
             move_down state (box_of_cell (address (r+1) c grid))
    |Some box2 -> if ((value box2) = value box) then
        let new_v = 2*value box in
        update_score state ((score state) + new_v);
        print_endline ("new_score " ^ (string_of_int (score state)));
        let b1 = remove_box r c grid in
        let b2 = remove_box (r+1) c b1 in
        let b3 = gen_box new_v (r+1) c b2 in
        (b3, score state) else (grid, score state)

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

let rec interface state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let next_move = read_line() in
  try
    let next_state  =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; exit 0
      |Up -> let (g, scr) = move_all_up state (grid state) in
        (new_state (random g) scr)
      |Down -> let (g, scr) = move_all_down state (grid state) in
        (new_state (random g) scr)
      |Left -> let (g, scr) = move_all_left state (grid state) in
        (new_state (random g) scr)
      |Right -> let (g, scr) = move_all_right state (grid state) in
        (new_state (random g) scr)
    in
    let next_grid = grid state in 
    if win next_grid then
      (print_endline "Congratulations! You win!"; exit 0)
    else if lose next_grid then
      (print_endline "Game Over"; exit 0)
    else interface next_state
  with
  | _ -> print_endline "You did something wrong, please try again" ; interface state


(*
  let rec interface state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let next_move = read_line() in
  try
    match(parse next_move) with
    |Quit -> print_endline "thank you for playing"; exit 0
    |Up -> let (g, scr) = move_all_up state (grid state) in
      interface (new_state (random g) scr)
    |Down -> let (g, scr) = move_all_down state (grid state) in
      print_endline ("down score " ^ (string_of_int scr));
      interface (new_state (random g) scr)
    |Left -> let (g, scr) = move_all_left state (grid state) in
      interface(new_state (random g) scr)
    |Right -> let (g, scr) = move_all_right state (grid state) in
      interface (new_state (random g) scr)
  with
  | _ -> print_endline "You did something wrong, please try again" ; interface state
*)

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game.\n");
  interface (init_state ())

(* Execute the game engine. *)
let () = main ()


