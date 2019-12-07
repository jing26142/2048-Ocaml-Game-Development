open State
open Grid
open Yojson.Basic.Util

type state_rep = {
  row0 : string;
  row1 : string;
  row2 : string;
  row3 : string;

}

let rec pow_10 a n =
  match n with
  |0 -> 1
  |_ -> pow_10 (a*10) (n-1)

let rec str_list row =
  match row with
  |[] -> ""
  |h::t -> string_of_int(h) ^ "," ^ (str_list t)


let string_save st =
  let grid_lst = to_matrix (grid st) in
  let row0 = str_list (List.nth grid_lst 0) in
  let row1 = str_list (List.nth grid_lst 1) in
  let row2 = str_list (List.nth grid_lst 2) in
  let row3 = str_list (List.nth grid_lst 3) in
  `Assoc [ ("row0", `String row0); ("row1"), `String row1;
           ("row2", `String row2); ("row3"), `String row3;
           ("score", `Int (score st)); ("gamelog", `String (gamelog st))]

