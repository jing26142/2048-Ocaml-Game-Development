(* TODO: command module structure *)
type command =

  | Up
  | Down
  | Left
  | Right
  | Quit
  | Player2 of int * int
  | GameMode1
  | GameMode2

exception Empty
exception Malformed


(* [parse_aux cleanlst] is a helper function that returns output of type command
   Raises [Empty] if cleanlst is empty
   Raises [Malformed] if input is not formatted according
   to the specification of parse*)




let parse_aux cleanlst bound =
  match cleanlst with
  | []-> raise Empty
  | h::t -> if h = "w" && List.length t = 0 then Up
    else if h = "quit" && List.length t = 0 then Quit else if
      h = "a" && List.length t =0 then Left else if h = "s" && List.length t = 0
    then Down else if h = "d" && List.length t =0 then Right
    else if h = "p" && List.length t =2 (*&& List.mem (List.nth t 0) bound && List.mem (List.nth t 1) bound*)
    then Player2 (int_of_string (List.nth t 0), int_of_string (List.nth t 1))else if h = "single" && List.length t = 0 then GameMode1 else if h = "multi" && List.length t = 0 then GameMode2
    else raise Malformed



(* [clean_input split_input lst] is a helper function
   that returns a list of the user input with all white space removed*)
let rec clean_input split_input lst =
  match split_input with
  | [] -> []
  | h::t -> if h = "" then clean_input t lst
    else List.rev_append [h] (clean_input t lst)

let parse str =
  let bound = ["0";"1";"2";"3"] in
  let split_input = String.split_on_char ' ' str in

  let clean_list = clean_input split_input [] in

  parse_aux clean_list bound









