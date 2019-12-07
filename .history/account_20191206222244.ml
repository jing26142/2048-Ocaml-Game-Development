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
  date_join : string;
  last_played: string;
}




let account_str st name score played dj lp= 
  let gamelog = gamelog st in
  `Assoc [
    ("name", `String name); ("last_score", `Int score);
    ("last_gamelog" , `String gamelog); ("games_played", `Int played);
    ("date_joined", `String dj);("last_played", `String lp)
  ]

let account_rep_of_json j = {
  name = j |> member "name" |> to_string;
  last_score = j |> member "last_score" |> to_int;
  last_gamelog = j |> member "last_gamelog" |> to_string;
  games_played = j |> member "games_played" |> to_int;
  last_played = j |> member "last_played" |> to_string;
}

let games_played acc =
  acc.games_played

let name acc =
  acc.name

let last_score acc =
  acc.last_score