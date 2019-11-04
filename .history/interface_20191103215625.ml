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

let grid1 = empty
let grid2 = gen_box 2 0 0 grid1
let grid3 = gen_box 2 0 2 grid2

let gen r c vbox grid =
  gen_box r c vbox grid

let rec move_right box grid =
  let vbox = value box in
  let bpos = pos box in
  let r = fst bpos in
  let c = snd bpos in
  if ( c = (grid_size grid) -1) then (print_endline "true"; grid)
  else (print_endline "!!"; match (address r (c+1) grid) with
    |None -> (display (to_matrix grid)); gen r (c+1) (vbox) grid |>
                                         remove_box r c |> 
                                         move_right (box_of_cell (address r (c+1) grid))
    |Some box2 -> if ((value box2) = value box) then
        let new_v = 2*value box in remove_box r c grid 
                                   |> remove_box r (c+1)
                                   |> gen_box new_v r (c+1)
      else grid)



let rec right_box grid cell =
  match cell with
  |None -> grid
  |Some box -> move_right box grid



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
                           (move_right (box_of_cell (address 0 1 (grid state))) (grid state)) 0)
  with
  | _ -> print_endline "You did something wrong, please try again" ; interface state




let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 2048 game.\n");
  interface (init_state ())

(* Execute the game engine. *)
let () = main ()


