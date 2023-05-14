open Flow

val generate : int -> board
(** [generate size] will return a valid board of size * size *)

val get_solution : unit -> board
(** [get_solution ()] will return a valid solution of the board previously
    generated. Only call this function after [generate size]. *)
