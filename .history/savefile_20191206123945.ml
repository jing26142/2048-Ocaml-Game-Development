open State
open Grid
open Yojson.Basic.Util

type t = {
  row0 : string;
  row1 : string;
  row2 : string;
  row3 : string;
  score : int;
  gamelog : string;
}


let rec str_list row =
  match row with
  |[] -> ""
  |h::t -> string_of_int(h) ^ "," ^ (str_list t)

let string_save st =
  let score = score st in
  let grid_lst = to_matrix (grid st) in
  let row0 = str_list (List.nth grid_lst 0) in
  let row1 = str_list (List.nth grid_lst 1) in
  let row2 = str_list (List.nth grid_lst 2) in
  let row3 = str_list (List.nth grid_lst 3) in
  `Assoc [ ("row0", `String row0); ("row1"), `String row1;
           ("row2", `String row2); ("row3"), `String row3;
           "score", `Int score; ("gamelog", `String (gamelog st))]

let state_rep_of_json j = {
  row0 = j |> member "row0" |> to_string;
  row1 = j |> member "row1" |> to_string;
  row2 = j |> member "row2" |> to_string;
  row3 = j |> member "row3" |> to_string;
  score = j |> member "score" |> to_int;
  gamelog = j |> member "gamelog" |> to_string
}

let rec str_to_intlst str n =
  match str with
  |"" -> []
  |_ -> [int_of_string(String.sub str 0 1)] @ 
        str_to_intlst (String.sub str 1 (n-1)) (n-1)

(* Turns integer into list *)
let rec int_to_int_list num lst n =
  match num with
  |0 -> []
  |_ -> [num mod n] @ (int_to_int_list

                       let crate_state sf =
                         failwith "Create a new state from save_point"