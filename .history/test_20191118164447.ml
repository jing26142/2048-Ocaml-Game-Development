open OUnit2
open Grid
open Interface
open State
open Grid

(*try*)

let cell_val = function
  |None -> None
  |Some b -> Some (value b)

(*let grid1 = empty
  let grid2 = gen_box 2 0 0 grid1
  let gridw = gen_box 2048 2 3 grid2
  let gridl = empty |> gen_box 2 0 0 |> gen_box 4 0 1 |> gen_box 8 0 2 |>
            gen_box 16 0 3 |> gen_box 32 1 0 |> gen_box 64 1 1 |>
            gen_box 128 1 2 |> gen_box 256 1 3 |> gen_box 512 2 1 |>
            gen_box 1024 2 2 |> gen_box 2 2 3 |> gen_box 4 3 0 |> gen_box 8 3 1
            |> gen_box 16 3 2 |> gen_box 32 3 3
  let grid3 = gridl |> remove_box 2 2
  let grid4 = gridl |> remove_box 2 3
  let grid5 = gridl |> remove_box 3 2
  let grid6 = grid3 |> gen_box 512 2 2
  let grid7 = grid3 |> gen_box 128 2 2
  let grid8 = gridl |> remove_box 1 3 |> gen_box 128 1 3
  let grid9 = gridl |> remove_box 3 2 |> gen_box 8 3 2
  let grid10 = gridl |> remove_box 3 1 |> gen_box 512 3 1
  let grid11 = gridl |> remove_box 2 3 |> gen_box 256 2 3*)

let grid_tests = [
  (*"non-empty sets contains 1" >:: (fun _ ->
      assert_equal (Some 2) (cell_val(address 0 0 grid2)));
    "size of grid" >:: (fun _ ->
      assert_equal (4) (grid_size grid1));
    (*"remove grid" >:: (fun _ ->
      assert_equal (None) ((remove_box 0 0 grid2); cell_val(address 0 0 grid2)));*)
    "check win condition - false" >:: (fun _ ->
      assert_equal false (win grid2));
    "check win condition - true" >:: (fun _ ->
      assert_equal true (win gridw));
    "check lose condition - false" >:: (fun _ ->
      assert_equal false (lose grid2));
    "check lose condition - empty cell in center" >:: (fun _ ->
      assert_equal false (lose grid3));
    "check lose condition - empty cell in rightmost column" >:: (fun _ ->
      assert_equal false (lose grid4));
    "check lose condition - empty cell in bottom row" >:: (fun _ ->
      assert_equal false (lose grid5));
    "check lose condition - horizontal duplicate in middle" >:: (fun _ ->
      assert_equal false (lose grid6));
    "check lose condition - vertical duplicate in middle" >:: (fun _ ->
      assert_equal false (lose grid7));
    "check lose condition - horizontal duplicate in rightmost column" >::
    (fun _ -> assert_equal false (lose grid8));
    "check lose condition - horizontal duplicate in bottommost row" >:: (fun _ ->
      assert_equal false (lose grid9));
    "check lose condition - vertical duplicate in bottommost row" >:: (fun _ ->
      assert_equal false (lose grid10));
    "check lose condition - vertical duplicate in rightmost row" >:: (fun _ ->
      assert_equal false (lose grid11));
    "check lose condition - true" >:: (fun _ ->
      assert_equal true (lose gridl));*)
  "check lose condition - true" >:: (fun _ ->
      assert_equal true (false));
]

let suite = "search test suite" >::: List.flatten[
    grid_tests
  ]

let _ = run_test_tt_main suite


