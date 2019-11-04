open OUnit2
open Grid
open Interface
open State

let grid1 = empty
let grid2 = gen_box grid1 2 0 0
let grid_tests = [
  "non-empty sets contains 1" >:: (fun _ ->
      assert_equal (Some 2) (address 0 0 grid2));
]

let suite = "search test suite" >::: List.flatten[
    grid_tests;
  ]

let _ = run_test_tt_main suite