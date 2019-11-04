open OUnit2
open Grid
open Interface
open State
open Grid

let cell_val = function
  |None -> None
  |Some b -> Some (value b)

let grid1 = empty
let grid2 = gen_box 2 0 0 grid1


let grid_tests = [
  "non-empty sets contains 1" >:: (fun _ ->
      assert_equal (Some 2) (cell_val(address 0 0 grid2)));
  "size of grid" >:: (fun _ ->
      assert_equal (4) (grid_size grid1)); 
  "remove grid" >:: (fun _ ->
      assert_equal (None) ((remove_box 0 0 grid2); cell_val(address 0 0 grid2)));
]

let suite = "search test suite" >::: List.flatten[
    grid_tests;
  ]

let _ = run_test_tt_main suite