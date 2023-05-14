open Flow
open Parsing

let generate_raw_file size =
  let size_string = string_of_int size in
  let cmd =
    "python numberlink/gen.py " ^ size_string ^ " " ^ size_string
    ^ " 1 --terminal-only --solve --no-pipes --zero --no-colors"
  in
  Sys.command cmd

let generate_json_file _ =
  let cmd = "python numberlink/gen_json.py " in
  Sys.command cmd

let generate (size : int) : board =
  let _ = generate_raw_file size in
  let _ = generate_json_file () in
  let board_json = Yojson.Basic.from_file "data/board.json" in
  let board = board_from_json board_json in
  board

let get_solution = fun () -> 
  let solution_json = Yojson.Basic.from_file "data/solution.json" in
  let solution = board_from_json solution_json in
  solution
