open Project
open Flow
open Visuals
open Command
open State
open Generate

(** Run this file using [dune exec bin/main.exe] or [make play] in the main
    project directory. *)

(** [play_flow board action] continues playing the game with the given user
    [board] and [action]. [action] is the input typed by the user, and the
    [board] is the current state of the game at the time they type the input
    (state before new change). [play_flow] draws a new line if the command is of
    [DrawLine] and the move is valid. If it is an [InvalidMove], prompt the user
    for another input. If the command is [StartNode], change the start node if
    its is valid. If it is an [InvalidNode], reprompt. [Clear] command restarts
    the game with the current board in its initial state (no pipes). If the
    command is [Malformed] or [Empty], than reprompt for a new command. The game
    should not quit unexpectedly. *)
let rec play_flow (board : board) (action : string) : unit =
  try
    match parse action with
    | DrawLine movetype -> (
        try
          let new_board = move board movetype in
          add_pipes new_board;
          if new_board.current_node = (~-1, ~-1) then
            print_endline "Pipe finished! Enter a new starting node:\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | action -> play_flow new_board action
        with InvalidMove -> (
          print_endline "Invalid move. Try again.\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | action -> play_flow board action))
    | StartNode start_node -> (
        try
          let start_board = start board start_node in
          add_pipes start_board;
          print_endline "To draw a line, use the command \"L | R | U | D\"\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | action -> play_flow start_board action
        with InvalidNode -> (
          print_endline "Invalid start node. Try again.\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | action -> play_flow board action))
    | Start -> (
        print_endline "Enter a valid start node:\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | action -> play_flow board action)
    | Undo -> (
        let undo_board = clear_pipe board in
        Graphics.clear_graph ();
        clear undo_board;
        print_endline "Enter a starting node:\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | action -> play_flow undo_board action)
    | Clear -> (
        let cleared_board = clear_board board in
        Graphics.clear_graph ();
        clear cleared_board;
        print_endline "Enter a starting node:\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | action -> play_flow cleared_board action)
    | Quit -> exit 0
  with
  | Command.Empty -> (
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | action -> play_flow board action)
  | Command.Malformed -> (
      print_endline "Invalid command.";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | action -> play_flow board action)

(** [play_game n] starts a flow game with board size n x n and in text method. *)
let play_game n =
  try
    let n_int = int_of_string n in
    print_endline
      ("Starting game with board size " ^ string_of_int n_int ^ " x "
     ^ string_of_int n_int ^ "\n");
    let board = generate n_int in
    graph_init_text board;
    print_endline
      ("Directions: This is a puzzle game where the goal is to connect each \
        pair of the different color circles together. There is only one \
        solution to each puzzle and it should fill the entire board (no space \
        is left empty). You cannot overlap pipes. To play the game, give \
        commands to advance a pipe. Commands are prompted by \"> \". The board \
        is " ^ string_of_int n_int ^ " x " ^ string_of_int n_int
     ^ ", where the bottom left corner is (0, 0). Draw a line between exactly \
        two squares by typing the command of the form \"L | R | D | U\", this \
        is direction of the line you want to draw. Designate new start node \
        with \"start (_, _)\". Type \"quit\" to quit the game, \"undo\" to \
        clear the current pipe and \"clear\" to clear the board.\n\n");
    print_endline "To start, type \"start\"\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | action -> play_flow board action
  with Failure s -> print_endline "Invalid board size."

(** [main_text ()] executes the game in the text method. *)
let main_text () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\n\n\n\n\n\n\n\
    \ Hello! This is our 3110 implementation of the game FlowFree! Created by  \
     Raina Hoffmann, Thuy Pham, and Jeana Hoffmann. Have fun!";
  print_endline
    "\n\n\
     This game has two play modes: a text-based command line format and a  \
     click and keyboard option. Type \"T\" for text and \"C\" for click.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s ->
      if s = "T" then (
        print_endline
          "Please enter the desired board size n to start playing! Boards \
           will  be n x n.\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | n -> play_game n)
      else if s = "C" then (
        print_endline
          "Please enter the desired board size n to start playing! Boards will \
           be n x n.\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | n ->
            print_endline
              ("Directions: This is a puzzle game where the goal is to \
                connect  each pair of the different color circles together. \
                There is  only one solution to each puzzle and it should fill \
                the entire  board (no space is left empty). You cannot overlap \
                pipes. To  play the game, give commands to advance a pipe. \
                Commands are  prompted by \"> \". The board is " ^ n ^ " x " ^ n
             ^ ", where the bottom left corner is (0, 0). Click a circle node  \
                to start. Use the keys  WASD to move Up, Left, Down, and \
                Right,  respectively. When you finish a pipe, click another \
                circle to  start a new pipe.\n\n");
            let graph = Visuals.graph_init () in
            Visuals.play graph)
      else print_endline "Invalid command.";
      exit 0

(** [main ()] executes the game in the click method, starting by initializing
    the welcome screen. *)
let main () =
  let graph = Visuals.graph_init () in
  Visuals.play graph

(** Execute the game. *)
let () = main ()
