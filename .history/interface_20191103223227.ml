open State
open Grid
open Command

(*[new_value grid] adds either a 2 or 4 to a random cell containing a zero *)
let rec new_value grid =
  failwith "unimplemented"





(*[display grid] prints a 4 by 4 grid with values of the current state *)
let display grid =
  failwith "unimplemented"
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

let rec move_right box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if (c = (grid_size grid) -1) then (print_endline "true"; grid)
  else (print_endline "!!"; match (address r (c+1) grid) with
    |None -> (print_endline (string_of_int r)); gen_box (vbox) r (c+1) grid |>
                                                remove_box r c |> 
                                                move_right (box_of_cell (address r (c+1) grid))
    |Some box2 -> if ((value box2) = value box) then
        let new_v = 2*value box in remove_box r c grid 
                                   |> remove_box r (c+1)
                                   |> gen_box new_v r (c+1)
      else grid)



let rec move_right box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if (c = (grid_size grid) -1) then (print_endline "true"; grid)
  else (print_endline "!!"; match (address r (c+1) grid) with
    |None -> (print_endline (string_of_int r)); gen_box (vbox) r (c+1) grid |>
                                                remove_box r c |> 
                                                move_right (box_of_cell (address r (c+1) grid))
    |Some box2 -> if ((value box2) = value box) then
        let new_v = 2*value box in remove_box r c grid 
                                   |> remove_box r (c+1)
                                   |> gen_box new_v r (c+1)
      else grid)



let rec right_box cell grid =
  match cell with
  |None -> grid
  |Some box -> move_right box grid

let move_all_right grid = 
  grid |> right_box (address 0 3 grid) |> right_box (address 0 2 grid)
  |>right_box (address 0 1 grid) |> right_box (address 0 0 grid)
  |> right_box (address 1 3 grid) |> right_box (address 1 2 grid)
  |>right_box (address 1 1 grid) |> right_box (address 1 0 grid)
  |> right_box (address 2 3 grid) |> right_box (address 2 2 grid)
  |>right_box (address 2 1 grid) |> right_box (address 2 0 grid)
  |> right_box (address 3 3 grid) |> right_box (address 3 2 grid)
  |>right_box (address 3 1 grid) |> right_box (address 3 0 grid)


let rec interface state =
  display (to_matrix (grid state));
  let next_move = read_line() in
  try
    match(parse next_move) with
    |Quit -> print_endline "thank you for playing"; exit 0; ()
    |Up -> print_endline "thank you for playing"; exit 0; ()
    |Down -> print_endline "thank you for playing"; exit 0; ()
    |Left ->print_endline "thank you for playing"; exit 0; ()
    |Right -> interface (new_state 
                           ((move_all_right (grid state))|> random)   0)
  with
  | _ -> print_endline "You did something wrong, please try again" ; interface state




let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game.\n");
  interface (init_state ())

(* Execute the game engine. *)
let () = main ()


