open State
open Grid
open Savefile
open Yojson.Basic.Util
open Command

type t = {
  name : string;
  games_played : int;
  last_score : int;
  last_gamelog : string;
}

let outacc jsn name =
  ANSITerminal.(print_string [red] 
                  ("Enter account name to be saved as\n"));
  let savename = read_line() in
  let jsnfile = open_out (savename^"_acc") in
  output_string jsnfile (Yojson.Basic.pretty_to_string jsn)


let account_str st name = 
  let score = score st in
  let gamelog = gamelog st in
  `Assoc [
    ("name", `String name); ("last_score", `Int score);
    ("last_gamelog" , `String gamelog); ("games_played", `Int 1)
  ]

let acc_rep_of_json j = {
  row0 = j |> member "row0" |> to_string;
  row1 = j |> member "row1" |> to_string;
  row2 = j |> member "row2" |> to_string;
  row3 = j |> member "row3" |> to_string;
  score = j |> member "score" |> to_int;
  gamelog = j |> member "gamelog" |> to_string
}