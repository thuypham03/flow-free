(** See flow.mli for specifications. *)

type node = int * int

type pipe = {
  start_end : node * node;
  color : int * int * int;
  path : node list;
}

type board = {
  size : int;
  pipe_list : pipe list;
  occupied : node list;
  current_pipe : pipe;
  current_node : node;
}

let pos_h node = fst node
let pos_v node = snd node
