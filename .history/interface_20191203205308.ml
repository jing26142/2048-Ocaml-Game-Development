open State
open Grid
open Command


(** FUNCTIONS FOR CPU-CONTROLLED PLAYER *)

(** [cpu_d0 st] is a copy of [st] with a box generated corresponding to
    difficulty level 0 for single-player mode. In difficulty level 0, the CPU
    generates a box in a random empty cell. *)
let cpu_d0 st =
  new_state (st |> grid |> random) (score st)

type dir = U | D | L |R

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
  new_state g_new (score st)

(*----------------------------------------------------------------------------*)

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

let rec string_row row =
  string_of_int(content_box row.(0))^ "|" ^string_of_int(content_box row.(1))^"|"^
  string_of_int(content_box row.(2))^ "|" ^string_of_int(content_box row.(3))^"|"


let string_rep grid =
  "----------------------------"^
  "\n|" ^ string_row (row 0 grid) ^
  "\n|" ^ string_row (row 1 grid) ^
  "\n|" ^ string_row (row 2 grid) ^
  "\n|" ^ string_row (row 3 grid)

let output log =
  let txtfile = open_out "./gamelog.txt" in
  output_string txtfile log;
  close_out txtfile;;

let rec p1_phase state str_rep =
  let next_move = read_line() in
  try
    let next_state  =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; output (string_rep (grid state));
        exit 0
      |Up -> let (g, scr) = move_all_up state (grid state) in
        (new_state g scr)
      |Down -> let (g, scr) = move_all_down state (grid state) in
        (new_state g scr)
      |Left -> let (g, scr) = move_all_left state (grid state) in
        (new_state g scr)
      |Right -> let (g, scr) = move_all_right state (grid state) in
        (new_state g scr)
      |_ -> print_endline "invalid player command"; p1_phase state str_rep
    in
    next_state
  with
  | _ -> print_endline "You did something wrong, please try again" ; p1_phase state

let rec p2_phase state =
  print_endline "Player 1's Turn";
  let state2 = p1_phase state in
  (* display (to_matrix (grid state2)); *)
  print_endline (string_rep (grid state2));
  let grid2 = grid state2 in
  if win grid2 then (print_endline "Congratulation Player 1. You Win!"; exit 0)
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
          (print_endline "Congratulation Player 2. You Win!"; exit 0)
        else
          (new_state fgrid (score state2))
    |_ -> display (to_matrix (grid state2)); (
        print_endline "Entered Wrong Command. Player 2 Try again"); p2_turn state2
  with
  |_-> print_endline "You did something wrong, please try again" ; p2_turn state2

let rec interface state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let st' = p1_phase state in
  let next_state = cpu_d1 st' in
  let next_grid = grid state in
  if win next_grid then
    (print_endline "Congratulations! You win!"; exit 0)
  else if lose next_grid then
    (print_endline "Game Over"; exit 0)
  else interface next_state

let rec interfaced0 state =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let st' = p1_phase state in
  let next_state = cpu_d0 st' in
  let next_grid = grid state in
  if win next_grid then
    (print_endline "Congratulations! You win!"; exit 0)
  else if lose next_grid then
    (print_endline "Game Over"; exit 0)
  else interface next_state

let rec interface2 state =
  display (to_matrix (grid state));
  p2_phase state |> interface2 

let rec chose_diff () =
  ANSITerminal.(print_string [red] "type d0 for easy mode and d1 for hard mode\n");
  let diff_choice = read_line() in
  match (parse diff_choice) with
  | Difficulty1 -> interfaced0(init_state ())
  | Difficulty2 -> interface(init_state ())
  | _ -> print_endline "invalid difficulty level, try again"; chose_diff ()


let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game. type single for 1 player or 
                  type multi for 2 player game mode\n");
  let game_choice = read_line() in
  match(parse game_choice) with
  | GameMode1 ->  chose_diff ()
  | GameMode2 -> interface2 (init_state ())
  | _ -> print_endline "You did something wrong, please try again"


(* Execute the game engine. *)
let () = main ()







