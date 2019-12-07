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
  let savename = read_line() in
  let jsnfile = open_out savename in
  output_string jsnfile ()


let account_str st name = 
  let score = score st in
  let gamelog = gamelog st in
  `Assoc [
    ("name", `String name); ("last_score", `Int score);
    ("last_gamelog" , `String gamelog); ("games_played", `Int 1)
  ]