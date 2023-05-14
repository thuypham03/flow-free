open OUnit2
open Project
open Flow
open Parsing
open Command
open State
open Graphics
open Printer

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let cmp_pipe pipe1 pipe2 = 
  pipe1.start_end = pipe2.start_end &&
  pipe1.color = pipe2.color &&
  cmp_set_like_lists pipe1.path pipe2.path

let rec cmp_pipe_list lst1 lst2 =
  match lst1, lst2 with
  | [],  [] -> true
  | h1 :: t1, h2 :: t2 -> (cmp_pipe h1 h2) && (cmp_pipe_list t1 t2)
  | _ -> false

let cmp_board board1 board2 = 
  board1.size = board2.size &&
  cmp_pipe_list board1.pipe_list board2.pipe_list && 
  cmp_set_like_lists board1.occupied board2.occupied &&
  cmp_pipe board1.current_pipe board2.current_pipe && 
  board1.current_node = board2.current_node

(* Tests for clear_board. *)
let clear_board_test (name : string) (current_board : board)
    (expected_output : board) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_board
    expected_output (State.clear_board current_board)

let pipe1 = { start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = [] }
let pipe1_finish = {
  pipe1 with path = [(0, 8); (0, 7); (0, 6); (0, 5); (0, 4); (0, 3)]
}
let pipe1_not_finish = {
  pipe1 with path = [(0, 8); (0, 7); (0, 6)]
}
let pipe2 = { start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = [] }
let pipe3 = { start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = [] }
let pipe4 = { start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = [] }
let pipe5 = { start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = [] }
let pipe5_finish = { 
  pipe5 with path = [(3, 3); (3, 4); (4, 4); (5, 4)]}
let pipe6 = { start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = [] }
let pipe7 = { start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [] }
let pipe8 = { start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = [] }

let empty_board =
  {
    size = 9;
    pipe_list = [ pipe1; pipe2; pipe3; pipe4; pipe5; pipe6; pipe7; pipe8 ];
    occupied = [];
    current_pipe = pipe1;
    current_node = (-1, -1);
  }

let board1 = {
  empty_board with
  pipe_list = [ pipe1_finish; pipe2; pipe3; pipe4; pipe5; pipe6; pipe7; pipe8 ];
  occupied = pipe1_finish.path;
  current_pipe = pipe1_finish;
}

let board1' = {
  empty_board with
  pipe_list = [ pipe1_not_finish; pipe2; pipe3; pipe4; pipe5; pipe6; pipe7; pipe8 ];
  occupied = pipe1_not_finish.path;
  current_pipe = pipe1_not_finish;
  current_node = (0, 6);
}

let board2 = {
  empty_board with
  pipe_list = [ pipe1_finish; pipe2; pipe3; pipe4; pipe5_finish; pipe6; pipe7; pipe8 ];
  occupied = pipe1_finish.path @ pipe5_finish.path;
  current_pipe = pipe5_finish;
}

let board_random = {
  size = 12;
  pipe_list =
  [{start_end = ((0, 11), (5, 10)); color = (201, 95, 197); path = []};
    {start_end = ((4, 10), (3, 0)); color = (173, 215, 123); path = []};
    {start_end = ((7, 10), (8, 1)); color = (13, 12, 13); path = []};
    {start_end = ((7, 9), (7, 7)); color = (209, 215, 221); path = []};
    {start_end = ((1, 6), (2, 4)); color = (255, 134, 85); path = []};
    {start_end = ((1, 5), (7, 4)); color = (192, 239, 55); path = []};
    {start_end = ((5, 5), (6, 2)); color = (29, 79, 235); path = []};
    {start_end = ((2, 3), (4, 2)); color = (161, 143, 172);
    path = [(0, 1); (0, 0); (1, 0); (1, 1); (1, 2); (1, 3); (2, 3)]}];
  occupied = [(0, 1); (0, 0); (1, 0); (1, 1); (1, 2); (1, 3); (2, 3)];
  current_pipe =
  {start_end = ((2, 3), (4, 2)); color = (161, 143, 172);
    path = [(0, 1); (0, 0); (1, 0); (1, 1); (1, 2); (1, 3); (2, 3)]};
  current_node = (0, 1)
}

let board_random5 = {
  size = 5;
   pipe_list =
    [{start_end = ((1, 3), (0, 1)); color = (152, 84, 235);
      path = [(1, 3); (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1)]};
     {start_end = ((2, 2), (3, 0)); color = (195, 62, 232);
      path =
       [(2, 2); (1, 2); (0, 2); (0, 3); (0, 4); (1, 4); (2, 4); (3, 4);
        (4, 4); (4, 3); (4, 2); (4, 1); (4, 0); (3, 0)]};
     {start_end = ((0, 0), (2, 0)); color = (239, 0, 156);
      path = [(0, 0); (1, 0); (2, 0)]}];
   occupied =
    [(1, 3); (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1);
     (2, 2); (1, 2); (0, 2); (0, 3); (0, 4); (1, 4); (2, 4); (3, 4);
     (4, 4); (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (0, 0); (1, 0);
     (2, 0)];
   current_pipe =
    {start_end = ((1, 3), (0, 1)); color = (152, 84, 235);
     path = [(1, 3); (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1)]};
   current_node = (-1, -1)
}

let board_random6 = {
  size = 5;
   pipe_list =
    [{start_end = ((4, 4), (0, 2)); color = (162, 221, 250);
      path = [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2)]};
     {start_end = ((4, 3), (3, 1)); color = (22, 149, 18);
      path = [(4, 3); (3, 3); (2, 3); (1, 3); (1, 2); (1, 1); (2, 1); (3, 1)]};
     {start_end = ((2, 2), (0, 1)); color = (86, 111, 225);
      path =
       [(2, 2); (3, 2); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0);
        (0, 0); (0, 1)]}];
   occupied =
    [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (4, 3);
     (3, 3); (2, 3); (1, 3); (1, 2); (1, 1); (2, 1); (3, 1); (2, 2);
     (3, 2); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0); (0, 0);
     (0, 1)];
   current_pipe =
    {start_end = ((4, 4), (0, 2)); color = (162, 221, 250);
     path = [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2)]};
   current_node = (-1, -1)
}

let clear_board_tests = [
  clear_board_test "clear an empty board" empty_board empty_board;
  clear_board_test "1 finished pipe" board1 empty_board;
  clear_board_test "unfinished pipe" board1' empty_board;
  clear_board_test "2 finished pipe" board2 {empty_board with current_pipe = pipe5};
  clear_board_test "random board" board_random 
  {
    size = 12;
    pipe_list =
      [{start_end = ((0, 11), (5, 10)); color = (201, 95, 197); path = []};
      {start_end = ((4, 10), (3, 0)); color = (173, 215, 123); path = []};
      {start_end = ((7, 10), (8, 1)); color = (13, 12, 13); path = []};
      {start_end = ((7, 9), (7, 7)); color = (209, 215, 221); path = []};
      {start_end = ((1, 6), (2, 4)); color = (255, 134, 85); path = []};
      {start_end = ((1, 5), (7, 4)); color = (192, 239, 55); path = []};
      {start_end = ((5, 5), (6, 2)); color = (29, 79, 235); path = []};
      {start_end = ((2, 3), (4, 2)); color = (161, 143, 172); path = []}];
    occupied = [];
    current_pipe =
      {start_end = ((2, 3), (4, 2)); color = (161, 143, 172); path = []};
    current_node = (-1, -1)
  };
  clear_board_test "random board" board_random5 {
    size = 5;
    pipe_list =
     [{start_end = ((1, 3), (0, 1)); color = (152, 84, 235); path = []};
      {start_end = ((2, 2), (3, 0)); color = (195, 62, 232); path = []};
      {start_end = ((0, 0), (2, 0)); color = (239, 0, 156); path = []}];
    occupied = [];
    current_pipe =
     {start_end = ((1, 3), (0, 1)); color = (152, 84, 235); path = []};
    current_node = (-1, -1)
  };
  clear_board_test "random board" board_random6 {
    size = 5;
    pipe_list =
     [{start_end = ((4, 4), (0, 2)); color = (162, 221, 250); path = []};
      {start_end = ((4, 3), (3, 1)); color = (22, 149, 18); path = []};
      {start_end = ((2, 2), (0, 1)); color = (86, 111, 225); path = []}];
    occupied = [];
    current_pipe =
     {start_end = ((4, 4), (0, 2)); color = (162, 221, 250); path = []};
    current_node = (-1, -1)
  };
]

(* Tests for clear_pipe. *)
let clear_pipe_test (name : string) (current_board : board)
    (expected_output : board) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_board 
    expected_output (State.clear_pipe current_board)

let clear_pipe_tests = [
  clear_pipe_test "clear an empty board" empty_board empty_board;
  clear_pipe_test "1 finished pipe" board1 empty_board;
  clear_pipe_test "unfinished pipe" board1' empty_board;
  clear_pipe_test "2 finished pipe" board2 {board1 with current_pipe = pipe5};
  clear_pipe_test "random board" board_random 
  {
    size = 12;
    pipe_list =
      [{start_end = ((0, 11), (5, 10)); color = (201, 95, 197); path = []};
      {start_end = ((4, 10), (3, 0)); color = (173, 215, 123); path = []};
      {start_end = ((7, 10), (8, 1)); color = (13, 12, 13); path = []};
      {start_end = ((7, 9), (7, 7)); color = (209, 215, 221); path = []};
      {start_end = ((1, 6), (2, 4)); color = (255, 134, 85); path = []};
      {start_end = ((1, 5), (7, 4)); color = (192, 239, 55); path = []};
      {start_end = ((5, 5), (6, 2)); color = (29, 79, 235); path = []};
      {start_end = ((2, 3), (4, 2)); color = (161, 143, 172); path = []}];
    occupied = [];
    current_pipe =
      {start_end = ((2, 3), (4, 2)); color = (161, 143, 172); path = []};
    current_node = (-1, -1)
  }
]

let board_random2 = {
  size = 20;
  pipe_list =
  [{start_end = ((19, 19), (7, 17)); color = (208, 26, 76); path = []};
    {start_end = ((5, 18), (14, 6)); color = (226, 39, 141); path = []};
    {start_end = ((6, 17), (8, 16)); color = (35, 144, 71); path = []};
    {start_end = ((4, 16), (6, 4)); color = (160, 215, 156); path = []};
    {start_end = ((7, 16), (15, 12)); color = (180, 59, 85); path = []};
    {start_end = ((9, 16), (15, 9)); color = (183, 27, 77); path = []};
    {start_end = ((10, 16), (12, 3)); color = (129, 95, 20); path = []};
    {start_end = ((9, 10), (9, 6)); color = (127, 70, 162); path = []};
    {start_end = ((10, 9), (7, 4)); color = (223, 217, 88); path = []};
    {start_end = ((15, 8), (11, 1)); color = (171, 37, 169); path = []};
    {start_end = ((15, 6), (13, 3)); color = (100, 54, 57); path = []};
    {start_end = ((15, 5), (11, 2)); color = (143, 31, 116); path = []};
    {start_end = ((10, 4), (1, 1)); color = (227, 25, 45); path = []}];
  occupied = [];
  current_pipe =
  {start_end = ((19, 19), (7, 17)); color = (208, 26, 76); path = []};
  current_node = (-1, -1)
}

let board_random3 = {
  size = 15;
  pipe_list =
   [{start_end = ((9, 13), (7, 7)); color = (249, 91, 25); path = []};
    {start_end = ((10, 13), (9, 9)); color = (239, 16, 205); path = []};
    {start_end = ((5, 12), (7, 12)); color = (176, 49, 136); path = []};
    {start_end = ((6, 12), (3, 10)); color = (82, 172, 141); path = []};
    {start_end = ((9, 11), (10, 7)); color = (36, 89, 0); path = []};
    {start_end = ((4, 8), (10, 2)); color = (110, 29, 75); path = []};
    {start_end = ((8, 8), (7, 6)); color = (205, 25, 100); path = []};
    {start_end = ((1, 7), (0, 5)); color = (12, 205, 20); path = []};
    {start_end = ((9, 7), (7, 3)); color = (184, 105, 247); path = []};
    {start_end = ((14, 3), (13, 0)); color = (85, 61, 83); path = []};
    {start_end = ((12, 2), (6, 1)); color = (251, 128, 221); path = []}];
  occupied = [];
  current_pipe =
   {start_end = ((9, 13), (7, 7)); color = (249, 91, 25); path = []};
  current_node = (-1, -1)
}

let board_random4 = {
  size = 5;
  pipe_list =
   [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
    {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = []};
    {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
  occupied = [];
  current_pipe =
   {start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
  current_node = (-1, -1)
}

(* Tests for start. *)
let start_test (name : string) (current_board : board) (start_node: node)
    (expected_output : board) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_board
    expected_output (State.start current_board start_node)

let start_test_fail (name : string) (current_board : board) (start_node: node): test =
  name >:: fun _ ->
    assert_raises (InvalidNode) (fun () -> State.start current_board start_node)

let start_tests = [
  start_test "choose start node" empty_board (1, 1) {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = []};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = [(1, 1)]};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = []};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(1, 1)];
    current_pipe =
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = [(1, 1)]};
    current_node = (1, 1)
  };
  start_test "choose end node" empty_board (2, 5) {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = []};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(2, 5)];
    current_pipe =
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
    current_node = (2, 5)
  };
  start_test_fail "invalid node (in board)" empty_board (0, 9);
  start_test_fail "invalid node (out board)" empty_board (-1, -1);
  start_test_fail "invalid node (visited node)" {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = []};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(2, 5)];
    current_pipe =
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
    current_node = (2, 5)
  } (2, 5);
  start_test "start 2 consecutive times"  {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = []};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(2, 5)];
    current_pipe =
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
    current_node = (2, 5)
  } (7, 0) {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2); path = []};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = [(2, 5)]};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = [(7, 0)]}];
    occupied = [(7, 0); (2, 5)];
    current_pipe =
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = [(7, 0)]};
    current_node = (7, 0)
  };
  start_test "random" board_random4 (2, 2) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = [(2, 2)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(2, 2)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = [(2, 2)]};
    current_node = (2, 2)
  };
  start_test "random" board_random4 (0, 0) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = [(0, 0)]};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = []};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(0, 0)];
    current_pipe =
     {start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = [(0, 0)]};
    current_node = (0, 0)
  };
  start_test "random" board_random4 (1, 3) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = []};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = [(1, 3)]}];
    occupied = [(1, 3)];
    current_pipe =
     {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = [(1, 3)]};
    current_node = (1, 3)
  };
  start_test "random" board_random4 (3, 4) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = [(3, 4)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(3, 4)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = [(3, 4)]};
    current_node = (3, 4)
  };
  start_test "random" board_random4 (2, 4) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = [(2, 4)]};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164); path = []};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(2, 4)];
    current_pipe =
     {start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = [(2, 4)]};
    current_node = (2, 4)
  };
]

(* Tests for move. *)
let move_test (name : string) (moved_board : board) 
    (expected_output : board) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_board
    expected_output moved_board

let move_test_fail (name : string) (current_board : board) (move: move_type): test =
  name >:: fun _ ->
    assert_raises (InvalidMove) (fun () -> State.move current_board move)

let pstart snode board = State.start board snode
let pmove move board = State.move board move

let move_tests = [
  move_test_fail "move without start" empty_board L;
  move_test_fail "move out of board" {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
        path = [(0, 0); (0, 1); (0, 2)]};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = []};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(0, 0); (0, 1); (0, 2)];
    current_pipe =
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
      path = [(0, 0); (0, 1); (0, 2)]};
    current_node = (0, 0)
  } D;
  move_test_fail "interfere with other node" {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230);
        path = [(0, 8); (0, 7); (0, 6); (0, 5); (0, 4); (0, 3)]};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
        path = [(1, 4); (1, 3); (1, 2); (0, 2)]};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184);
        path = [(3, 3); (3, 4); (4, 4); (5, 4)]};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = []};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied =
      [(1, 4); (1, 3); (1, 2); (0, 2); (0, 8); (0, 7); (0, 6); (0, 5); (0, 4);
      (0, 3); (3, 3); (3, 4); (4, 4); (5, 4)];
    current_pipe =
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
      path = [(1, 4); (1, 3); (1, 2); (0, 2)]};
    current_node = (1, 4)
  } L;
  move_test "random" (board2 |> pstart (0,2) |> pmove R |> pmove U |> pmove U) {
    size = 9;
    pipe_list =
      [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230);
        path = [(0, 8); (0, 7); (0, 6); (0, 5); (0, 4); (0, 3)]};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
        path = [(1, 4); (1, 3); (1, 2); (0, 2)]};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184);
        path = [(3, 3); (3, 4); (4, 4); (5, 4)]};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = []};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied =
      [(1, 4); (1, 3); (1, 2); (0, 2); (0, 8); (0, 7); (0, 6); (0, 5); (0, 4);
      (0, 3); (3, 3); (3, 4); (4, 4); (5, 4)];
    current_pipe =
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
      path = [(1, 4); (1, 3); (1, 2); (0, 2)]};
    current_node = (1, 4)
  };
  move_test "random" (empty_board |> pstart (0, 2) |> pmove D |> pmove D) {
    size = 9;
    pipe_list =
     [{start_end = ((0, 8), (0, 3)); color = (2, 14, 230); path = []};
      {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
       path = [(0, 0); (0, 1); (0, 2)]};
      {start_end = ((1, 1), (5, 3)); color = (130, 6, 196); path = []};
      {start_end = ((8, 0), (3, 2)); color = (212, 205, 6); path = []};
      {start_end = ((3, 3), (5, 4)); color = (4, 169, 184); path = []};
      {start_end = ((2, 3), (5, 5)); color = (209, 120, 4); path = []};
      {start_end = ((7, 1), (2, 5)); color = (89, 23, 13); path = []};
      {start_end = ((7, 0), (3, 6)); color = (201, 4, 4); path = []}];
    occupied = [(0, 0); (0, 1); (0, 2)];
    current_pipe =
     {start_end = ((0, 2), (5, 1)); color = (14, 92, 2);
      path = [(0, 0); (0, 1); (0, 2)]};
    current_node = (0, 0)
  };
  move_test "random" (board_random2 |> pstart (10, 9) |> pmove L |> pmove D |> pmove D |> pmove R |> pmove R) {
    size = 20;
    pipe_list =
      [{start_end = ((19, 19), (7, 17)); color = (208, 26, 76); path = []};
      {start_end = ((5, 18), (14, 6)); color = (226, 39, 141); path = []};
      {start_end = ((6, 17), (8, 16)); color = (35, 144, 71); path = []};
      {start_end = ((4, 16), (6, 4)); color = (160, 215, 156); path = []};
      {start_end = ((7, 16), (15, 12)); color = (180, 59, 85); path = []};
      {start_end = ((9, 16), (15, 9)); color = (183, 27, 77); path = []};
      {start_end = ((10, 16), (12, 3)); color = (129, 95, 20); path = []};
      {start_end = ((9, 10), (9, 6)); color = (127, 70, 162); path = []};
      {start_end = ((10, 9), (7, 4)); color = (223, 217, 88);
        path = [(11, 7); (10, 7); (9, 7); (9, 8); (9, 9); (10, 9)]};
      {start_end = ((15, 8), (11, 1)); color = (171, 37, 169); path = []};
      {start_end = ((15, 6), (13, 3)); color = (100, 54, 57); path = []};
      {start_end = ((15, 5), (11, 2)); color = (143, 31, 116); path = []};
      {start_end = ((10, 4), (1, 1)); color = (227, 25, 45); path = []}];
    occupied = [(11, 7); (10, 7); (9, 7); (9, 8); (9, 9); (10, 9)];
    current_pipe =
      {start_end = ((10, 9), (7, 4)); color = (223, 217, 88);
      path = [(11, 7); (10, 7); (9, 7); (9, 8); (9, 9); (10, 9)]};
    current_node = (11, 7)
  };
  move_test "random" (board_random2 |> pstart (15, 6) |> pmove R |> pmove R |> pmove U |> pmove U |> pmove L) {
    size = 20;
    pipe_list =
      [{start_end = ((19, 19), (7, 17)); color = (208, 26, 76); path = []};
      {start_end = ((5, 18), (14, 6)); color = (226, 39, 141); path = []};
      {start_end = ((6, 17), (8, 16)); color = (35, 144, 71); path = []};
      {start_end = ((4, 16), (6, 4)); color = (160, 215, 156); path = []};
      {start_end = ((7, 16), (15, 12)); color = (180, 59, 85); path = []};
      {start_end = ((9, 16), (15, 9)); color = (183, 27, 77); path = []};
      {start_end = ((10, 16), (12, 3)); color = (129, 95, 20); path = []};
      {start_end = ((9, 10), (9, 6)); color = (127, 70, 162); path = []};
      {start_end = ((10, 9), (7, 4)); color = (223, 217, 88); path = []};
      {start_end = ((15, 8), (11, 1)); color = (171, 37, 169); path = []};
      {start_end = ((15, 6), (13, 3)); color = (100, 54, 57);
        path = [(16, 8); (17, 8); (17, 7); (17, 6); (16, 6); (15, 6)]};
      {start_end = ((15, 5), (11, 2)); color = (143, 31, 116); path = []};
      {start_end = ((10, 4), (1, 1)); color = (227, 25, 45); path = []}];
    occupied = [(16, 8); (17, 8); (17, 7); (17, 6); (16, 6); (15, 6)];
    current_pipe =
      {start_end = ((15, 6), (13, 3)); color = (100, 54, 57);
      path = [(16, 8); (17, 8); (17, 7); (17, 6); (16, 6); (15, 6)]};
    current_node = (16, 8)
  };
  move_test "random" (board_random3 |> pstart (4, 8) |> pmove R |> pmove R |> pmove U |> pmove U |> pmove L) {
    size = 15;
    pipe_list =
      [{start_end = ((9, 13), (7, 7)); color = (249, 91, 25); path = []};
      {start_end = ((10, 13), (9, 9)); color = (239, 16, 205); path = []};
      {start_end = ((5, 12), (7, 12)); color = (176, 49, 136); path = []};
      {start_end = ((6, 12), (3, 10)); color = (82, 172, 141); path = []};
      {start_end = ((9, 11), (10, 7)); color = (36, 89, 0); path = []};
      {start_end = ((4, 8), (10, 2)); color = (110, 29, 75);
        path = [(5, 10); (6, 10); (6, 9); (6, 8); (5, 8); (4, 8)]};
      {start_end = ((8, 8), (7, 6)); color = (205, 25, 100); path = []};
      {start_end = ((1, 7), (0, 5)); color = (12, 205, 20); path = []};
      {start_end = ((9, 7), (7, 3)); color = (184, 105, 247); path = []};
      {start_end = ((14, 3), (13, 0)); color = (85, 61, 83); path = []};
      {start_end = ((12, 2), (6, 1)); color = (251, 128, 221); path = []}];
    occupied = [(5, 10); (6, 10); (6, 9); (6, 8); (5, 8); (4, 8)];
    current_pipe =
      {start_end = ((4, 8), (10, 2)); color = (110, 29, 75);
      path = [(5, 10); (6, 10); (6, 9); (6, 8); (5, 8); (4, 8)]};
    current_node = (5, 10)
  };
  move_test "random" (board_random3 |> pstart (6, 12) |> pmove D |> pmove L |> pmove D) {
    size = 15;
    pipe_list =
      [{start_end = ((9, 13), (7, 7)); color = (249, 91, 25); path = []};
      {start_end = ((10, 13), (9, 9)); color = (239, 16, 205); path = []};
      {start_end = ((5, 12), (7, 12)); color = (176, 49, 136); path = []};
      {start_end = ((6, 12), (3, 10)); color = (82, 172, 141);
        path = [(5, 10); (5, 11); (6, 11); (6, 12)]};
      {start_end = ((9, 11), (10, 7)); color = (36, 89, 0); path = []};
      {start_end = ((4, 8), (10, 2)); color = (110, 29, 75); path = []};
      {start_end = ((8, 8), (7, 6)); color = (205, 25, 100); path = []};
      {start_end = ((1, 7), (0, 5)); color = (12, 205, 20); path = []};
      {start_end = ((9, 7), (7, 3)); color = (184, 105, 247); path = []};
      {start_end = ((14, 3), (13, 0)); color = (85, 61, 83); path = []};
      {start_end = ((12, 2), (6, 1)); color = (251, 128, 221); path = []}];
    occupied = [(5, 10); (5, 11); (6, 11); (6, 12)];
    current_pipe =
      {start_end = ((6, 12), (3, 10)); color = (82, 172, 141);
      path = [(5, 10); (5, 11); (6, 11); (6, 12)]};
    current_node = (5, 10)
  };
  move_test "random" (board_random4 |> pstart (3, 4) |> pmove R) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
       path = [(4, 4); (3, 4)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(4, 4); (3, 4)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
      path = [(4, 4); (3, 4)]};
    current_node = (4, 4)
  };
  move_test_fail "random" (board_random4 |> pstart (3, 4)) L;
  move_test_fail "random" (board_random4 |> pstart (3, 4)) U;
  move_test "random" (board_random4 |> pstart (3, 4) |> pmove D) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
       path = [(3, 3); (3, 4)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(3, 3); (3, 4)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
      path = [(3, 3); (3, 4)]};
    current_node = (3, 3)
  };
  move_test_fail "random" (board_random4 |> pstart (2, 2)) D;
  move_test "random" (board_random4 |> pstart (2, 2) |> pmove U) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
       path = [(2, 3); (2, 2)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(2, 3); (2, 2)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
      path = [(2, 3); (2, 2)]};
    current_node = (2, 3)
  };
  move_test "random" (board_random4 |> pstart (2, 2) |> pmove L) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
       path = [(1, 2); (2, 2)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(1, 2); (2, 2)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
      path = [(1, 2); (2, 2)]};
    current_node = (1, 2)
  };
  move_test "random" (board_random4 |> pstart (2, 2) |> pmove R) {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 0)); color = (246, 94, 65); path = []};
      {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
       path = [(3, 2); (2, 2)]};
      {start_end = ((1, 3), (2, 1)); color = (52, 120, 90); path = []}];
    occupied = [(3, 2); (2, 2)];
    current_pipe =
     {start_end = ((3, 4), (2, 2)); color = (34, 128, 164);
      path = [(3, 2); (2, 2)]};
    current_node = (3, 2)
  };
]

let state_test_suit = List.flatten
  [ start_tests; move_tests; clear_pipe_tests; clear_board_tests ]
