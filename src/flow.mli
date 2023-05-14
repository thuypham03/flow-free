(** Representation of static flow free data.

    This module represents the data stored in flow free files, including the
    board, nodes, and pipe data. It handles loading of that data from our board
    algorithm as well as querying the data. *)

type node = int * int
(** The type of values representing nodes in flow free graph. A node is any
    place on a graph that can hold pipe. A node is represented as a point, where
    [(0,0)] is the bottom left square of the grid*)

type pipe = {
  start_end : node * node;
  color : int * int * int;
  path : node list;
}
(** The type of values representing a single pipe in a flow free graph. A [pipe]
    is comprised of a start and end node with a [path] connecting the two
    circles. It's [path] is made up of a list of nodes that reach from the start
    node towards the end node. This [path] may or may not be complete in a given
    board state. The [path] is the order of player movement with the first
    element being the first move by the player from that starting pipe node. For
    example in a straight line from node (0, 3) to node (0,7) the [path] will be
    [(0, 3);(0,4);(0,5);(0,6);(0,7)]. The first node of [path] will always be
    the start node and when the pipe is finished the last node of [path] will be
    the end node, both specified in [start_end]. [color] is a triple rgb value *)

type board = {
  size : int;
  pipe_list : pipe list;
  occupied : node list;
  current_pipe : pipe;
  current_node : node;
}
(** The type of values representing a single board of a flow free game. [size]
    gives the dimension of the board which is always square ([size] x [size]).
    [pipe_list] is a list of Pipes to be connected on the board. [occupied] is
    the list of nodes which currently are occupied by pipe and cannot be
    traversed over. [current_pipe] is the current pipe the user is drawing.
    [current_node] is the node the user is currently drawing from*)

val pos_h : node -> int
(**getter function that returns the horizontal position of the node, with 0
   being on the left*)

val pos_v : node -> int
(**getter function that returns the vertical position of the node, with 0 being
   on the bottom*)
