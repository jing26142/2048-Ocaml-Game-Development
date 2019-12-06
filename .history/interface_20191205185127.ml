open State
open Grid
open Command
open Printf
open Pervasives

type dir = State.dir

let update_leaderboard () name score = 
  let message = String.concat " " [">";name;"-";score;"<"] in 
  let file = "scorelog.rtf" in
  let oc = open_out_gen [Open_append] 0 file in    

  fprintf oc "%s\n" message;   
  close_out oc;  

  exit 0

let final_details () = 
  print_endline "Please enter your name"; 
  let name = read_line() in 
  print_endline "Please enter your final score"; 
  let final_score = read_line() in 

  update_leaderboard () name final_score 

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



let output log =
  let txtfile = open_out "./gamelog.txt" in
  output_string txtfile log;
  close_out txtfile;;

let new_log st =
  ((gamelog st)^ "\n" ^ (string_rep (grid st)))

(** FUNCTIONS FOR CPU-CONTROLLED PLAYER (AI UNITS) *)

(** [cpu_d0 st] is a copy of [st] with a box generated corresponding to
    difficulty level 0 for single-player mode. In difficulty level 0, the CPU
    generates a box in a random empty cell. *)
let cpu_d0 st =
  let g = (st|>grid|>random) in
  new_state g (score st) ((gamelog st) ^ "\n\nCPU's move: " ^ (string_rep g))


let best_tile_d1_dir g i j dir =
  let Some box1 = (address i j g) in
  let (di, dj) =
    match dir with
    | U -> (-1, 0)
    | D -> (1, 0)
    | L -> (0, -1)
    | R -> (0, 1)
  in
  match address (i + di) (j + dj) g with
  | Some _ -> []
  | None ->
    begin
      match address (i + 2 * di) (j + 2 * dj) g with
      | Some box2 ->
        if value box1 = value box2 then
          [(value box1, (i + di, j + dj))]
        else
          []
      | None ->
        let edge_cond =
          begin
            match dir with
            | U -> i = 3
            | D -> i = 0
            | L -> j = 3
            | R -> j = 0
          end
        in
        if edge_cond then
          begin
            match address (i + 3 * di) (j + 3 * dj) g with
            | Some box2 ->
              if (value box1) = (value box2) then
                [(value box1, (i + di, j + dj));
                 (value box1, (i + 2 * di, j + 2 * dj))]
              else
                []
            | None -> []
          end
        else []
    end

(** [best_of_tile_d1 g i j] is [(sc, (r, c))] where [r] and [c] is the row and
    column to place a box to prevent a merge with box in cell (i, j) in [g] and
    [sc] is the score of the merge that would be prevented if a box were placed
    at the corresponding location. If there is no such position, outputs
    (0, (0, 0)). *)
let best_tile_d1 g i j =
  match address i j g with
  | None -> []
  | Some box ->
    let vert =
      if i < 2 then
        best_tile_d1_dir g i j D
      else
        best_tile_d1_dir g i j U
    in
    let hori =
      if j < 2 then
        best_tile_d1_dir g i j R
      else
        best_tile_d1_dir g i j L
    in
    vert @ hori

let comp_addr (_, (i1, j1)) (_, (i2, j2)) =
  let i_comp = compare i1 i2 in
  if i_comp = 0 then
    compare j1 j2
  else
    i_comp

let merge_pos acc (v1, (i1, j1)) =
  match acc with
  | [] -> [(v1, (i1, j1))]
  | (v2, (i2, j2)) :: t as lst ->
    if (i1, j1) = (i2, j2) then
      (v1 + v2, (i1, j1)) :: t
    else
      (v1, (i1, j1)) :: lst

let get_max arr =
  let lst_fl = arr |> Array.to_list |> List.flatten |> List.sort comp_addr |>
               List.fold_left merge_pos [] in
  let max = List.fold_left (fun acc x -> if fst x > fst acc then x else acc)
      (0, (0, 0)) lst_fl in
  if max = (0, (0, 0)) then None else Some max

(** [cpu_d1 st] is a copy of [st] with a box generated corresponding to
    difficulty level 1 for single-player mode. In difficulty level 1, the CPU
    attempts to block the biggest preventable merge possible in the next player
    move. *)
let cpu_d1 st =
  let g = grid st in
  let options = Array.make 16 [] in
  let _ = for i = 0 to 3 do
      for j = 0 to 3 do
        options.(4 * i + j) <- best_tile_d1 g i j
      done
    done in
  let g_new =
    match get_max options with
    | None -> random g
    | Some (_, (i, j)) -> gen_box 2 i j g
  in
  new_state g_new (score st) ((gamelog st) ^ "\n\nCPU's move: " ^ (string_rep g_new))

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
(*----------------------------------------------------------------------------*)

(*[new_value grid] adds either a 2 or 4 to a random cell containing a zero *)
let rec new_value grid =
  failwith "unimplemented"



let rec p1_phase state =
  let next_move = read_line() in
  try
    let next_state  =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; 
        output ((gamelog state)^ "\n" ^ (string_rep (grid state))); final_details();
      | Up -> move_all state U
      | Down -> move_all state D
      | Left -> move_all state L
      | Right -> move_all state R
      |_ -> print_endline "invalid player command"; p1_phase state
    in
    let g = grid next_state in
    let scr = score next_state in
    (new_state g scr ((gamelog state)^ "\nP1's move:" ^ (string_rep (grid state))))
  with
  | _ -> print_endline "You did something wrong, please try again" ; p1_phase state

let rec p2_phase state =
  print_endline "Player 1's Turn";
  let state2 = p1_phase state in
  display (to_matrix (grid state2));
  let grid2 = grid state2 in
  if win grid2 then 
    (print_endline "Congratulation Player 1. You Win!";
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); exit 0)
  else
    print_endline "Player 2's turn";
  p2_turn state2

and p2_turn state2 =
  try
    let next_move = read_line() in
    match (parse next_move)  with
    |Quit -> print_endline "thank you for playing the game"; exit 0
    |Player2 (r, c) -> if (r>3 || r<0 || c>3 || c<0) then (
        display (to_matrix (grid state2)); (
          print_endline "Entered Wrong Command. Player 2 Try again");
        p2_turn state2
      )
      else
      if (not (is_empty_box (content_box (address r c (grid state2)))))
      then (display (to_matrix (grid state2)); (
          print_endline "The entered location is not empty.Please try again.");
         p2_turn state2
        )
      else
        let fgrid = gen_box 2 r c (grid state2) in
        if lose fgrid then
          (print_endline "Congratulation Player 2. You Win!";
           output ((gamelog state2)^ "\n" ^ (string_rep (grid state2))); exit 0)
        else 
          (new_state fgrid (score state2) 
             ((gamelog state2)^ "\n\nP2's move:" ^ (string_rep (grid state2))))
    |_ -> display (to_matrix (grid state2)); (
        print_endline "Entered Wrong Command. Player 2 Try again"); p2_turn state2
  with
  |_-> print_endline "You did something wrong, please try again" ; p2_turn state2

let match_diff d = 
  if d = 0 then cpu_d0
  else if d = 1 then cpu_d1
  else failwith "invalid difficulty"

let rec interface state d =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let st' = p1_phase state in
  let next_state = (match_diff d) st' in
  let next_grid = grid state in
  if win next_grid then
    (print_endline "Congratulations! You win!";  
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); exit 0)
  else if lose next_grid then
    (print_endline "Game Over";
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); exit 0)
  else interface next_state d

let rec interface2 state =
  display (to_matrix (grid state));
  p2_phase state |> interface2 

let rec rev_phase state d =
  print_endline "CPU's Turn";
  let state2 = cpu_p1 state d in
  display (to_matrix (grid state2));
  let grid2 = grid state2 in
  if win grid2 then 
    (print_endline "Game Over";  
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); exit 0)
  else
    print_endline "Your turn";
  p2_turn state2

let rec interface3 state d = 
  display (to_matrix (grid state));
  interface3 (rev_phase state d) d

let rec chose_diff () =
  ANSITerminal.(print_string [red] "type d0 for easy mode and d1 for hard mode\n");
  let diff_choice = read_line() in
  match (parse diff_choice) with
  | Difficulty1 -> interface (init_state ()) 0
  | Difficulty2 -> interface (init_state ()) 1
  | _ -> print_endline "invalid difficulty level, try again"; chose_diff ()

let rec choose_diff3 () =
  ANSITerminal.(print_string [red] "type d0 for easy mode and d1 for hard mode\n");
  let diff_choice = read_line () in 
  match (parse diff_choice) with
  | Difficulty1 -> interface3 (init_state ()) 1
  | Difficulty2 -> interface3 (init_state ()) 2
  | _ -> print_endline "invalid difficulty level, try again"; choose_diff3 ()  

let read_file () = 
  let file = "scorelog.rtf" in

  let in_channel = open_in file in
  try
    while true do
      let line = input_line in_channel in
      if (String.contains line '{' =false) && (String.contains line '>') then 
        let final_string = String.sub line (String.index line '>') ((String.index line '<')-(String.index line '>')) in 
        print_endline (final_string);

        (* do something with line *)
    done
  with End_of_file ->

    print_endline "type single for 1 player or type multi for 2 player game mode";
    let game_choice = read_line() in
    match(parse game_choice) with
    | GameMode1 ->  chose_diff ()
    | GameMode2 -> interface2 (init_state ())

    | _ -> print_endline "You did something wrong, please try again" 


let main () =
  ANSITerminal.(print_string [red] (
      "\n\nWelcome to the 2048 game. type single for 1 player or" ^ 
      "type multi for 2 player game mode or reverse\n"^
      "\nType scorelog to see where you stand"));
  let game_choice = read_line() in
  match(parse game_choice) with
  | GameMode1 ->  chose_diff ()
  | GameMode2 -> interface2 (init_state ())
  | GameMode3 -> choose_diff3 ()
  | Scorelog -> read_file()
  | _ -> print_endline "You did something wrong, please try again"


(* Execute the game engine. *)
let () = main ()