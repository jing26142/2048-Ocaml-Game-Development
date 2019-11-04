 
type t = {
  current_grid : Grid.t;
  score : int
}

let init_state () =
  failwith "Unimplemented"

let grid st =
  st.current_grid

let score st =
  st.score
