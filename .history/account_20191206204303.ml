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
  let savename = read_line()^"_acc" in
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
  name = j |> member "name" |> to_string;
  last_score = j |> member "last_score" |> to_int;
  last_gamelog = j |> member "last_gamelog" |> to_string;
  games_played = j |> member "games_played" |> to_int;
}