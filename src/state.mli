open Flow
(** Representation of dynamic board state. This module represents the state of
    an board as it is being played, including the board's current room, the
    nodes that have been occupied, and functions that cause the state to change. *)

type move_type =
  | L
  | R
  | U
  | D

exception InvalidNode
(** Exception raised when the node is invalid: out of board, has previously been
    visited before *)

exception InvalidMove
(** Exception raised when the move is invalid: out of board, interfere with
    other pipe *)

val start : board -> node -> board
(** [start cur_board next_node] will start draw a line in a pipe, and return a
    new board with that node added. If the node is out of board, not the
    beginning of any pipe, or has previously been visited, it will raise
    exception [InvalidNode] *)

val move : board -> move_type -> board
(** [move cur_board next_move] will start moving in [next_move] direction. If
    the move is invalid (move out of board, interfere with other pipes, move when
    haven't start a new pipe) then it will raise exception [InvalidMove]. If the 
    pipe is finished, then it return the board with [current_node = (-1, -1)] *)

val clear_pipe : board -> board
(** [clear_pipe cur_board] will clear the current pipe in the board *)

val clear_board : board -> board
(** [clear_board cur_board] will clear whole board to its initial state *)
