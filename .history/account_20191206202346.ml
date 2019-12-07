open State
open Grid
open Savefile
open Yojson.Basic.Util
open Command

type t = {
  name : string;
  games_played : int;
  last_game : string;
}