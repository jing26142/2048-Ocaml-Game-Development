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


let rec single_player state = 
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
      |_ -> print_endline "invalid player command"; single_player state
    in
    next_state
  with
  | _ -> print_endline "You did something wrong, please try again" ; single_player state

let rec p1_phase state = 
  let next_move = read_line() in
  try 
    let next_state  =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; exit 0
      |Up -> let (g, scr) = move_all_up state (grid state) in
        (new_state g scr)
      |Down -> let (g, scr) = move_all_down state (grid state) in
        (new_state g scr)
      |Left -> let (g, scr) = move_all_left state (grid state) in
        (new_state g scr)
      |Right -> let (g, scr) = move_all_right state (grid state) in
        (new_state g scr)
      |_ -> print_endline "invalid player command"; p1_phase state
    in
    next_state
  with
  | _ -> print_endline "You did something wrong, please try again" ; p1_phase state

let rec p2_phase state =
  let state2 = p1_phase state in
  display (to_matrix (grid state2));
  let grid2 = grid state2 in
  if win grid2 then (print_endline "Congratulation Player 1. You Win!"; exit 0)
  else
    let next_move = read_line() in
    try
      let rec p2_turn state =
        match (parse next_move)  with
        |Quit -> print_endline "thank you for playing the game"; exit 0
        |Player2 (r, c) -> let fgrid = gen_box 2 r c (grid state2) in 
          display (to_matrix fgrid);
          if lose fgrid then 
            (print_endline "Congratulation Player 2. You Win!"; exit 0)
          else
            (new_state fgrid (score state))
        |_ -> p2_turn state
      in p2_turn state |> p2_phase
    with
    |_-> print_endline "You did something wrong, please try again" ; p2_phase state

let rec interface state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let next_state = single_player state in
  let next_grid = grid state in 
  if win next_grid then
    (print_endline "Congratulations! You win!"; exit 0)
  else if lose next_grid then
    (print_endline "Game Over"; exit 0)
  else interface next_state

let rec interface2 state =
  display (to_matrix (grid state));
  p2_phase state

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game. type single for 1 player or type multi for 2 player game mode\n");
  let game_choice = read_line() in
  match(parse game_choice) with
  | GameMode1 -> interface (init_state ())
  | GameMode2 -> interface2 (init_state ())
  | _ -> print_endline "You did something wrong, please try again"


(* Execute the game engine. *)
let () = main ()
