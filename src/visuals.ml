open Flow
open State

let buffer = 60
let background_color = Graphics.rgb 31 28 28
let welcome_screen_color = Graphics.rgb 255 204 204
let easy = Graphics.rgb 102 255 102
let med = Graphics.rgb 255 178 102
let hard = Graphics.rgb 225 102 102
let drawing_color = Graphics.rgb 107 69 69
let line_width = 2

type mode =
  | EASY
  | MEDIUM
  | HARD

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

(* [draw lines range size space corner] will draw lines in range starting from
   the bottom left corner. [range] is a list from 0 -- size. [size] is the
   dimension of the board. [space] is the space between grid lines. [corner] is
   the bottom left corner of the grid to be drawn. *)
let rec draw_lines (range : int list) (size : int) (space : int)
    (corner : int * int) =
  match range with
  | [] -> ()
  | h :: t ->
      Graphics.moveto (fst corner) (snd corner + (h * (space + line_width)));
      Graphics.rlineto (size * (space + line_width)) 0;
      Graphics.moveto (fst corner + (h * (space + line_width))) (snd corner);
      Graphics.rlineto 0 (size * (space + line_width));
      Graphics.moveto (fst corner) (snd corner);
      draw_lines t size space corner

(* [bottom_left size space] will return the bottom left corner of the grid.
   Where [size] is the dimension of the grid and [space] is the width of 1
   square of the grid. *)
let bottom_left size space =
  let x = Graphics.size_x () in
  let y = Graphics.size_y () in
  let center = (x / 2, y / 2) in
  if size mod 2 = 1 then
    ( fst center - ((size / 2 * (space + line_width)) + (space / 2)),
      snd center - ((size / 2 * (space + line_width)) + (space / 2)) )
  else
    ( fst center - (size / 2 * (space + line_width)),
      snd center - (size / 2 * (space + line_width)) )

(* [add_grid size] will add a square grid of dimension [size] x [size] to the
   center of the window. *)
let add_grid size =
  let x = Graphics.size_x () in
  let range = 0 -- size in
  let space = (x / size) - (2 * buffer / size) in
  let corner = bottom_left size space in
  Graphics.moveto (fst corner) (snd corner);
  draw_lines range size space corner

(* [ft trip] Returns the first element of a triple [trip]. *)
let ft (x, _, _) = x

(* [sd trip] Returns the second element of a triple [trip]. *)
let sd (_, y, _) = y

(* [td trip] Returns the third element of a triple [trip]. *)
let td (_, _, z) = z

(* [calc_radius size] calculates the radius of the pipe circles in a grid given
   the [size] of the grid. *)
let calc_radius size =
  let x = Graphics.size_x () in
  int_of_float (float_of_int ((x / size) - (2 * buffer / size)) /. 2.5)

(* [draw_path board space path] will draw the path of a pipe. [board] is the
   current board. [space] is the width of 1 square in the grid. [path] is the
   path of the pipe. See flow.mli for more on path types. *)
let rec draw_path (board : board) (space : int) (path : node list) =
  match path with
  | [] -> ()
  | start :: next :: tail ->
      let bl = bottom_left board.size space in
      let x = fst bl + (pos_h start * (space + line_width)) + (space / 2) in
      let y = snd bl + (pos_v start * (space + line_width)) + (space / 2) in

      Graphics.moveto x y;
      let pipe_width = 20 * 10 / board.size in
      Graphics.set_line_width pipe_width;

      let start_h = pos_h start in
      let start_v = pos_v start in
      let next_h = pos_h next in
      let next_v = pos_v next in

      let move_h =
        if next_h > start_h then 1 else if next_h < start_h then -1 else 0
      in
      let move_v =
        if next_v > start_v then 1 else if next_v < start_v then -1 else 0
      in

      Graphics.rlineto
        (move_h * (space + line_width))
        (move_v * (space + line_width));

      draw_path board space (next :: tail)
  | h :: t -> ()

(* [add_pipes board] draws a list of pipes given a [board]. *)
let rec add_pipes board =
  let pipes = board.pipe_list in
  match pipes with
  | [] -> ()
  | h :: t ->
      let rad = calc_radius board.size in
      let s = fst h.start_end in
      let e = snd h.start_end in
      let color = h.color in
      let space =
        (Graphics.size_x () / board.size) - (2 * buffer / board.size)
      in
      let gcolor = Graphics.rgb (ft color) (sd color) (td color) in
      let bl = bottom_left board.size space in

      Graphics.set_color gcolor;

      let center1_x = fst bl + (pos_h s * (space + line_width)) + (space / 2) in
      let center1_y = snd bl + (pos_v s * (space + line_width)) + (space / 2) in

      let center2_x = fst bl + (pos_h e * (space + line_width)) + (space / 2) in
      let center2_y = snd bl + (pos_v e * (space + line_width)) + (space / 2) in

      Graphics.fill_circle center1_x center1_y rad;
      Graphics.fill_circle center2_x center2_y rad;

      let path = List.rev h.path in

      draw_path board space path;
      add_pipes { board with pipe_list = t }

let rec find_pipe (cur_node : node) = function
  | [] -> None
  | h :: t ->
      let s, e = h.start_end in
      if s = cur_node || e = cur_node then Some h else find_pipe cur_node t

(* [get_mode ()] waits on the welcome screen and returns the mode for the next
   board once the player clicks a button: EASY, MEDIUM, or HARD. *)
let rec get_mode () : mode =
  let status = Graphics.wait_next_event [ Button_down ] in
  if status.button then
    let mouse_pos = Graphics.mouse_pos () in
    let x = fst mouse_pos in
    let y = snd mouse_pos in
    if x >= 100 && x <= 150 && y >= 200 && y <= 250 then EASY
    else if x >= 375 && x <= 425 && y >= 200 && y <= 250 then MEDIUM
    else if x >= 650 && x <= 700 && y >= 200 && y <= 250 then HARD
    else get_mode ()
  else get_mode ()

(* [get_rand_number n] returns a psuedo-random number between 0 and n-1. *)
let get_rand_number (n : int) : int =
  int_of_float (Unix.gettimeofday () *. 1000.) mod n

let welcome_screen : board =
  Graphics.open_graph "";
  (* Open graph for welcome screen. *)
  Graphics.set_window_title "Flow Free";
  let dim_x = 800 in
  let dim_y = 800 in
  Graphics.resize_window dim_x dim_y;
  Graphics.set_color background_color;
  Graphics.fill_rect 0 0 800 800;
  (* Add text box to introduce game. *)
  Graphics.moveto 350 600;

  Graphics.set_color Graphics.white;
  Graphics.draw_string "WELCOME TO FLOW :)";
  Graphics.moveto 115 260;
  Graphics.draw_string "EASY";
  Graphics.moveto 382 260;
  Graphics.draw_string "MEDIUM";
  Graphics.moveto 664 260;
  Graphics.draw_string "HARD";
  Graphics.moveto 200 515;
  Graphics.draw_string
    "DIRECTIONS: This is a puzzle game where the goal is to connect each";
  Graphics.moveto 200 500;
  Graphics.draw_string "pair of the different color circles together.";
  Graphics.moveto 220 470;
  Graphics.draw_string ">  There is only one solution to each puzzle";
  Graphics.moveto 220 455;
  Graphics.draw_string ">  Pipes should fill the entire board";
  Graphics.moveto 220 440;
  Graphics.draw_string ">  You cannot overlap pipes";
  Graphics.moveto 220 425;
  Graphics.draw_string ">  Click a circle node to start";
  Graphics.moveto 220 410;
  Graphics.draw_string ">  Use the keys WASD to move Up, Left, Down, and Right";
  Graphics.moveto 220 395;
  Graphics.draw_string ">  To clear the current pipe, click a circle.";
  Graphics.moveto 220 380;
  Graphics.draw_string ">  To clear the board press \"O\".";

  (*easy button*)
  Graphics.set_color easy;
  Graphics.fill_rect 100 200 50 50;

  (*medium button*)
  Graphics.set_color med;
  Graphics.fill_rect 375 200 50 50;

  (*hard button*)
  Graphics.set_color hard;
  Graphics.fill_rect 650 200 50 50;

  let mode = get_mode () in
  match mode with
  | EASY -> Generate.generate (5 + get_rand_number 4)
  | MEDIUM -> Generate.generate (9 + get_rand_number 4)
  | HARD -> Generate.generate (13 + get_rand_number 3)

let add_solution_button () =
  Graphics.set_color hard;
  Graphics.fill_rect 36 10 95 20;
  Graphics.set_color Graphics.white;
  Graphics.moveto 45 15;
  Graphics.draw_string "view solution"

let clear (board : board) =
  Graphics.set_color background_color;
  Graphics.fill_rect 0 0 800 800;
  Graphics.set_color drawing_color;
  Graphics.set_line_width line_width;
  add_grid board.size;
  add_pipes board

let graph_init () =
  let board = welcome_screen in
  Graphics.clear_graph ();
  (* Might replace line with .clear to save window. *)
  Graphics.set_window_title "Flow Free";
  let dim_x = 800 in
  let dim_y = 800 in
  Graphics.resize_window dim_x dim_y;
  Graphics.set_color background_color;
  Graphics.fill_rect 0 0 dim_x dim_y;
  Graphics.set_color drawing_color;
  Graphics.set_line_width line_width;
  add_grid board.size;
  add_pipes board;
  add_solution_button ();
  board

let graph_init_text (board : board) =
  Graphics.open_graph "";
  Graphics.set_window_title "Flow Free";
  let dim_x = 800 in
  let dim_y = 800 in
  Graphics.resize_window dim_x dim_y;
  Graphics.set_color background_color;
  Graphics.fill_rect 0 0 dim_x dim_y;
  Graphics.set_color drawing_color;
  Graphics.set_line_width line_width;
  add_grid board.size;
  add_pipes board

let rec play (board : board) =
  let status = Graphics.wait_next_event [ Button_down; Key_pressed ] in
  let new_board =
    try
      if
        (*if mouse was pressed, then find new current node and current pipe*)
        status.button
      then
        let mouse_pos = Graphics.mouse_pos () in
        let x = fst mouse_pos in
        let y = snd mouse_pos in
        if x >= 36 && x <= 131 && y >= 10 && y <= 30 then (
          let empty = State.clear_board board in
          clear empty;
          Graphics.set_color (Graphics.rgb 178 2 2);
          Graphics.fill_rect 36 10 95 20;
          Graphics.set_color Graphics.white;
          Graphics.moveto 45 15;
          Graphics.draw_string "view solution";
          let solution = Generate.get_solution () in
          solution)
        else
          let space =
            (Graphics.size_x () / board.size) - (2 * buffer / board.size)
          in
          let h = (x - buffer) / (space + line_width) in
          let v = (y - buffer) / (space + line_width) in

          if board.current_node <> (-1, -1) then (
            let new_board2 = State.clear_pipe board in
            clear (State.start new_board2 (h, v));
            add_solution_button ();
            State.start new_board2 (h, v))
          else State.start board (h, v)
        (*if button L R U D was pressed, call state with that movement *)
      else if status.keypressed then
        match status.key with
        | 'w' -> State.move board U
        | 'a' -> State.move board L
        | 's' -> State.move board D
        | 'd' -> State.move board R
        | 'o' ->
            let empty = State.clear_board board in
            clear empty;
            add_solution_button ();
            empty
        | _ -> board
      else (
        add_solution_button ();
        board)
    with e -> board
  in
  add_pipes new_board;
  play new_board
