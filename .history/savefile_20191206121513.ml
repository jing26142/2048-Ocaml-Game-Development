open State
open Grid
open Yojson.Basic.Util

type state_rep = {
  row0 : int;
  row1 : int;
  row2 : int;
  row3 : int;
  score : int;
  gamelog : string;
}



let rec pow_10 a n =
  match n with
  |0 -> a
  |_ -> pow_10 (a*10) (n-1)

let rec int_row row n =
  match row with
  |[] -> 0
  |h::t -> (pow_10 h n) + int_row t (n-1)

let string_save st =
  let score = score st in
  let grid_lst = to_matrix (grid st) in
  let row0 = int_row (List.nth grid_lst 0) 3 in
  let row1 = int_row (List.nth grid_lst 1) 3 in
  let row2 = int_row (List.nth grid_lst 2) 3 in
  let row3 = int_row (List.nth grid_lst 3) 3 in
  `Assoc [ ("row0", `Int row0); ("row1"), `Int row1;
           ("row2", `Int row2); ("row3"), `Int row3;
           "score", `Int score; ("gamelog", `String (gamelog st))]

let state_rep_of_json j = {
  row0 = j |> member "row0" |> to_int;
  row1 = j |> member "row1" |> to_int;
  row2 = j |> member "row2" |> to_int;
  row3 = j |> member "row3" |> to_int;
  score = j |> member "score" |> to_int;
  gamelog = j |> member "gamelog" |> to_string
}


