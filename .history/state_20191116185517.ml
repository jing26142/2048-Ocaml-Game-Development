open Grid
type t = {
  current_grid : Grid.t;
  mutable score : int
}

let init_state () =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  let r21 = Random.int 4 in
  let r22 = Random.int 4 in
  let r21 = if (r21 = r11 && r22 = r22) then (r11+1) mod 4 else r21 in
  let grid = empty in {
    current_grid = gen_box 2 r11 r12 grid |> gen_box 2 r21 r22;
    score = 0
  }


let grid st =
  st.current_grid

let score st =
  st.score

let new_state  new_grid score =
  {current_grid = new_grid; score = 0}


let update_score st new_score =
  st.score <- new_score