(** A module for the visual depiction of flow free. Below are the functions that
    can be called. *)

val buffer : int
(** [buffer] is a ring of width [buffer] around the window of which will not be
    drawn over by the grid. *)

val background_color : Graphics.color
(** [background_color] is the background color of the graphics window. *)

val drawing_color : Graphics.color
(** [drawing_color] is the drawing color of the graphics window. *)

val add_pipes : Flow.board -> unit
(** [add_pipes board] will draw the pipes of a game given a [board]. *)

val graph_init : unit -> Flow.board
(** [graph_init ()] will draw a board of a game given selection made in the
    welcome screen. *)

val graph_init_text : Flow.board -> unit
(** [graph_init_text board] will draw a board of a game given a [board]. *)

val play : Flow.board -> 'a
(** [play board] will play a game given a [board]. *)

val clear : Flow.board -> unit
(** [clear board] will redraw the state of a [board]. *)
