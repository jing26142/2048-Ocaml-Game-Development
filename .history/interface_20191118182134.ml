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


let rec p1_phase state = 
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
      |_ -> print_endline "invalid player command"; p1_phase state
    in
    next_state
  with
  | _ -> print_endline "You did something wrong, please try again" ; p1_phase state

let rec p2_phase state =
  let state2 = p1_phase state in
  let next_move = read_line() in
  try
    match (parse next_move)  with
    |Quit -> print_endline "thank you for playing the game"; exit 0
    |Player2 (r, c) -> let fgrid = gen_box 2 r c (grid state2) in 
      p2_phase (new_state fgrid (score state))
    |_ -> p2_phase state
  with
  |_-> print_endline "You did something wrong, please try again" ; p2_phase state

let rec interface state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let next_state = p1_phase state in
  let next_grid = grid state in 
  if win next_grid then
    (print_endline "Congratulations! You win!"; exit 0)
  else if lose next_grid then
    (print_endline "Game Over"; exit 0)
  else interface next_state



let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game.\n");
  interface (init_state ())

(* Execute the game engine. *)
let () = main ()


