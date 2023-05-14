open OUnit2
open Project
open Flow
open Parsing
open Command
open State
open Graphics
open Printer

let data_dir_prefix = "data" ^ Filename.dir_sep ^ "test_data" ^ Filename.dir_sep

let board_from_json_test (name : string) (file_name : string)
    (expected_output : board) : test =
  name >:: fun _ ->
  let board_json = Yojson.Basic.from_file (data_dir_prefix ^ file_name) in
  assert_equal expected_output (Parsing.board_from_json board_json)

let parsing_test_suit = [
  board_from_json_test "test1" "test1.json" {
    size = 5;
    pipe_list =
      [{start_end = ((4, 4), (0, 2)); color = (134, 140, 13); path = []};
      {start_end = ((0, 3), (2, 2)); color = (123, 35, 213); path = []};
      {start_end = ((2, 3), (3, 1)); color = (111, 83, 229); path = []}];
    occupied = [];
    current_pipe =
      {start_end = ((4, 4), (0, 2)); color = (134, 140, 13); path = []};
    current_node = (-1, -1)
  };
  board_from_json_test "test2" "test2.json" {
    size = 8;
    pipe_list =
      [{start_end = ((2, 7), (3, 0)); color = (236, 235, 50); path = []};
      {start_end = ((7, 7), (5, 5)); color = (3, 169, 253); path = []};
      {start_end = ((2, 5), (4, 4)); color = (152, 163, 250); path = []};
      {start_end = ((7, 5), (4, 3)); color = (165, 16, 55); path = []};
      {start_end = ((6, 3), (5, 1)); color = (214, 177, 156); path = []}];
    occupied = [];
    current_pipe =
      {start_end = ((2, 7), (3, 0)); color = (236, 235, 50); path = []};
    current_node = (-1, -1)
  };
  board_from_json_test "test3" "test3.json" {
    size = 10;
    pipe_list =
     [{start_end = ((3, 8), (9, 0)); color = (34, 115, 247); path = []};
      {start_end = ((3, 7), (9, 3)); color = (243, 59, 23); path = []};
      {start_end = ((5, 7), (4, 4)); color = (67, 41, 95); path = []};
      {start_end = ((6, 7), (7, 6)); color = (40, 86, 51); path = []};
      {start_end = ((0, 6), (2, 3)); color = (141, 109, 56); path = []};
      {start_end = ((3, 6), (8, 6)); color = (246, 170, 202); path = []};
      {start_end = ((5, 5), (8, 5)); color = (197, 108, 160); path = []}];
    occupied = [];
    current_pipe =
     {start_end = ((3, 8), (9, 0)); color = (34, 115, 247); path = []};
    current_node = (-1, -1)
  };
  board_from_json_test "test4" "test4.json" {
    size = 15;
    pipe_list =
     [{start_end = ((0, 14), (7, 12)); color = (34, 228, 17); path = []};
      {start_end = ((7, 11), (6, 9)); color = (125, 33, 124); path = []};
      {start_end = ((9, 11), (7, 7)); color = (218, 25, 189); path = []};
      {start_end = ((5, 10), (6, 2)); color = (141, 93, 230); path = []};
      {start_end = ((8, 10), (12, 9)); color = (133, 174, 1); path = []};
      {start_end = ((9, 10), (10, 8)); color = (61, 255, 56); path = []};
      {start_end = ((13, 7), (6, 1)); color = (1, 191, 231); path = []};
      {start_end = ((10, 5), (6, 3)); color = (114, 148, 32); path = []};
      {start_end = ((4, 4), (2, 1)); color = (246, 185, 143); path = []};
      {start_end = ((7, 3), (4, 0)); color = (99, 192, 135); path = []}];
    occupied = [];
    current_pipe =
     {start_end = ((0, 14), (7, 12)); color = (34, 228, 17); path = []};
    current_node = (-1, -1)
  };
  board_from_json_test "test5" "test5.json" {
    size = 20;
    pipe_list =
     [{start_end = ((10, 19), (8, 3)); color = (157, 71, 197); path = []};
      {start_end = ((2, 17), (2, 12)); color = (239, 96, 146); path = []};
      {start_end = ((10, 16), (4, 15)); color = (172, 202, 135); path = []};
      {start_end = ((14, 16), (15, 15)); color = (18, 87, 79); path = []};
      {start_end = ((15, 16), (9, 2)); color = (37, 27, 112); path = []};
      {start_end = ((9, 15), (4, 14)); color = (95, 240, 19); path = []};
      {start_end = ((10, 15), (10, 10)); color = (205, 81, 15); path = []};
      {start_end = ((4, 13), (5, 12)); color = (125, 112, 190); path = []};
      {start_end = ((4, 12), (10, 9)); color = (148, 135, 47); path = []};
      {start_end = ((2, 11), (7, 5)); color = (147, 87, 252); path = []};
      {start_end = ((3, 9), (7, 3)); color = (138, 43, 152); path = []};
      {start_end = ((7, 4), (8, 2)); color = (160, 195, 76); path = []};
      {start_end = ((9, 4), (12, 0)); color = (241, 251, 69); path = []}];
    occupied = [];
    current_pipe =
     {start_end = ((10, 19), (8, 3)); color = (157, 71, 197); path = []};
    current_node = (-1, -1)
  };
  board_from_json_test "test6" "test6.json" {
    size = 5;
    pipe_list =
     [{start_end = ((4, 4), (0, 2)); color = (128, 143, 169);
       path = [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2)]};
      {start_end = ((4, 3), (3, 1)); color = (22, 157, 143);
       path = [(4, 3); (3, 3); (2, 3); (1, 3); (1, 2); (1, 1); (2, 1); (3, 1)]};
      {start_end = ((2, 2), (0, 1)); color = (36, 250, 243);
       path =
        [(2, 2); (3, 2); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0);
         (0, 0); (0, 1)]}];
    occupied =
     [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (4, 3); (3, 3);
      (2, 3); (1, 3); (1, 2); (1, 1); (2, 1); (3, 1); (2, 2); (3, 2); (4, 2);
      (4, 1); (4, 0); (3, 0); (2, 0); (1, 0); (0, 0); (0, 1)];
    current_pipe =
     {start_end = ((4, 4), (0, 2)); color = (128, 143, 169);
      path = [(4, 4); (3, 4); (2, 4); (1, 4); (0, 4); (0, 3); (0, 2)]};
    current_node = (-1, -1)
  };
  board_from_json_test "test7" "test7.json" {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (0, 3)); color = (48, 3, 79);
       path = [(2, 4); (1, 4); (0, 4); (0, 3)]};
      {start_end = ((3, 4), (0, 2)); color = (248, 134, 166);
       path =
        [(3, 4); (4, 4); (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0);
         (1, 0); (0, 0); (0, 1); (0, 2)]};
      {start_end = ((3, 3), (2, 1)); color = (12, 71, 100);
       path = [(3, 3); (2, 3); (1, 3); (1, 2); (1, 1); (2, 1)]};
      {start_end = ((2, 2), (3, 1)); color = (185, 51, 102);
       path = [(2, 2); (3, 2); (3, 1)]}];
    occupied =
     [(2, 4); (1, 4); (0, 4); (0, 3); (3, 4); (4, 4); (4, 3); (4, 2); (4, 1);
      (4, 0); (3, 0); (2, 0); (1, 0); (0, 0); (0, 1); (0, 2); (3, 3); (2, 3);
      (1, 3); (1, 2); (1, 1); (2, 1); (2, 2); (3, 2); (3, 1)];
    current_pipe =
     {start_end = ((2, 4), (0, 3)); color = (48, 3, 79);
      path = [(2, 4); (1, 4); (0, 4); (0, 3)]};
    current_node = (-1, -1)
  };
  board_from_json_test "test8" "test8.json" {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (2, 2)); color = (105, 146, 195);
       path = [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2)]};
      {start_end = ((3, 4), (1, 0)); color = (163, 253, 214);
       path =
        [(3, 4); (4, 4); (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0)]};
      {start_end = ((1, 3), (0, 0)); color = (15, 43, 114);
       path =
        [(1, 3); (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1); (0, 0)]}];
    occupied =
     [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2); (3, 4); (4, 4);
      (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0); (1, 3); (2, 3);
      (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1); (0, 0)];
    current_pipe =
     {start_end = ((2, 4), (2, 2)); color = (105, 146, 195);
      path = [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2)]};
    current_node = (-1, -1)
  };
  board_from_json_test "test9" "test9.json" {
    size = 5;
    pipe_list =
     [{start_end = ((2, 4), (2, 2)); color = (132, 115, 168);
       path = [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2)]};
      {start_end = ((3, 4), (0, 0)); color = (103, 209, 71);
       path =
        [(3, 4); (4, 4); (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0);
         (1, 0); (0, 0)]};
      {start_end = ((1, 3), (0, 1)); color = (215, 175, 233);
       path = [(1, 3); (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1)]}];
    occupied =
     [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2); (3, 4); (4, 4);
      (4, 3); (4, 2); (4, 1); (4, 0); (3, 0); (2, 0); (1, 0); (0, 0); (1, 3);
      (2, 3); (3, 3); (3, 2); (3, 1); (2, 1); (1, 1); (0, 1)];
    current_pipe =
     {start_end = ((2, 4), (2, 2)); color = (132, 115, 168);
      path = [(2, 4); (1, 4); (0, 4); (0, 3); (0, 2); (1, 2); (2, 2)]};
    current_node = (-1, -1)
  };
  board_from_json_test "test10" "test10.json" {
    size = 4;
    pipe_list =
     [{start_end = ((2, 2), (3, 0)); color = (80, 119, 85);
       path = [(2, 2); (1, 2); (1, 1); (1, 0); (2, 0); (3, 0)]};
      {start_end = ((2, 1), (0, 0)); color = (81, 0, 196);
       path =
        [(2, 1); (3, 1); (3, 2); (3, 3); (2, 3); (1, 3); (0, 3); (0, 2);
         (0, 1); (0, 0)]}];
    occupied =
     [(2, 2); (1, 2); (1, 1); (1, 0); (2, 0); (3, 0); (2, 1); (3, 1); (3, 2);
      (3, 3); (2, 3); (1, 3); (0, 3); (0, 2); (0, 1); (0, 0)];
    current_pipe =
     {start_end = ((2, 2), (3, 0)); color = (80, 119, 85);
      path = [(2, 2); (1, 2); (1, 1); (1, 0); (2, 0); (3, 0)]};
    current_node = (-1, -1)
  };
]