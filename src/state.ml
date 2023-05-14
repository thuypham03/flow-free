open Flow

exception InvalidNode
exception InvalidMove

type move_type =
  | L
  | R
  | U
  | D

let rec is_visited (cur_node : node) = function
  | [] -> false
  | h :: t -> if h = cur_node then true else is_visited cur_node t

let is_valid (cur_board : board) (next_node : node) =
  let h, v = next_node in
  if not (0 <= h && h < cur_board.size && 0 <= v && v < cur_board.size) then
    false
  else not (is_visited next_node cur_board.occupied)

let rec find_pipe (cur_node : node) = function
  | [] -> None
  | h :: t ->
      let s, e = h.start_end in
      if s = cur_node || e = cur_node then Some h else find_pipe cur_node t

let rec append (next_node : node) (cur_pipe : pipe) = function
  | [] -> raise InvalidNode
  | h :: t ->
      if h.color <> cur_pipe.color then h :: append next_node cur_pipe t
      else
        let new_pipe = { h with path = next_node :: h.path } in
        new_pipe :: t

let visit (cur_board : board) (next_node : node) =
  let new_occupied = next_node :: cur_board.occupied in
  let new_pipe_list =
    append next_node cur_board.current_pipe cur_board.pipe_list
  in
  {
    size = cur_board.size;
    pipe_list = new_pipe_list;
    occupied = new_occupied;
    current_node = next_node;
    current_pipe =
      {
        cur_board.current_pipe with
        path = next_node :: cur_board.current_pipe.path;
      };
  }

let check_end (cur_board : board) =
  let s, e = cur_board.current_pipe.start_end in
  is_visited s cur_board.occupied && is_visited e cur_board.occupied

let rec is_start_end_pipe_helper (next_node : node) (start_end : node * node) =
  function
  | [] -> false
  | h :: t ->
      is_start_end_pipe_helper next_node start_end t
      ||
      if h.start_end = start_end then false
      else
        let s, e = h.start_end in
        next_node = s || next_node = e

let is_start_end_pipe (cur_board : board) (next_node : node) =
  is_start_end_pipe_helper next_node cur_board.current_pipe.start_end
    cur_board.pipe_list

let play (cur_board : board) (next_node : node) =
  if is_start_end_pipe cur_board next_node then raise InvalidMove
  else
    let new_board = visit cur_board next_node in
    if check_end new_board then { new_board with current_node = (-1, -1) }
    else new_board

let start (cur_board : board) (next_node : node) =
  if not (is_valid cur_board next_node) then raise InvalidNode
  else
    match find_pipe next_node cur_board.pipe_list with
    | None -> raise InvalidNode
    | Some cur_pipe ->
        let new_board = { cur_board with current_pipe = cur_pipe } in
        play new_board next_node

let get_next_node (cur_node : node) (next_move : move_type) =
  let h, v = cur_node in
  match next_move with
  | L -> (h - 1, v)
  | R -> (h + 1, v)
  | U -> (h, v + 1)
  | D -> (h, v - 1)

let move (cur_board : board) (next_move : move_type) =
  let next_node = get_next_node cur_board.current_node next_move in
  if not (is_valid cur_board next_node) then raise InvalidMove
  else play cur_board next_node

let rec empty_pipe s = function
  | [] -> []
  | h :: t ->
      if fst h.start_end = s then { h with path = [] } :: t
      else h :: empty_pipe s t

let clear_pipe (cur_board : board) =
  let cur_pipe = cur_board.current_pipe in
  let new_occupied =
    List.filter (fun elt -> not (List.mem elt cur_pipe.path)) cur_board.occupied
  in
  let new_pipe_list = empty_pipe (fst cur_pipe.start_end) cur_board.pipe_list in
  {
    cur_board with
    pipe_list = new_pipe_list;
    occupied = new_occupied;
    current_pipe = { cur_pipe with path = [] };
    current_node = (-1, -1);
  }

let rec empty_pipe_list = function
  | [] -> []
  | h :: t -> { h with path = [] } :: empty_pipe_list t

let clear_board (cur_board : board) =
  let cur_pipe = cur_board.current_pipe in
  let new_pipe_list = empty_pipe_list cur_board.pipe_list in
  {
    cur_board with
    pipe_list = new_pipe_list;
    occupied = [];
    current_pipe = { cur_pipe with path = [] };
    current_node = (-1, -1);
  }
