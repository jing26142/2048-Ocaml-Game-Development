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
  date_joined : string;
  last_played: string;
  all_scores : string;
  title : string;
}




let account_str st name score played dj lp sc title= 
  let gamelog = gamelog st in
  `Assoc [
    ("name", `String name); ("last_score", `Int score);
    ("last_gamelog" , `String gamelog); ("games_played", `Int played);
    ("date_joined", `String dj); ("last_played", `String lp);
    ("all_scores", `String sc); ("title", `String title)
  ]

let account_rep_of_json j = {
  name = j |> member "name" |> to_string;
  last_score = j |> member "last_score" |> to_int;
  last_gamelog = j |> member "last_gamelog" |> to_string;
  games_played = j |> member "games_played" |> to_int;
  date_joined = j |> member "last_gamelog" |> to_string;
  last_played = j |> member "last_played" |> to_string;
  all_scores = j |> member "all_scores" |> to_string;
  title = j |> member "title" |> to_string;
}

let games_played acc =
  acc.games_played

let name acc =
  acc.name

let last_score acc =
  acc.last_score

let date_joined acc =
  acc.date_joined

let last_played acc =
  acc.last_played

let all_scores acc =
  acc.all_scores

let title acc =
  acc.title