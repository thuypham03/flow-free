(** Parsing of player commands. *)

type phrase = State.move_type
(** The type [phrase] represents the phrase section of a player command. The
    points are parsed by the parentheses and transformed into an OCaml tuple.
    Examples:

    - If the command is "L", then the phrase is L. *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibily a phrase. Invariant: the [phrase] carried by [DrawLine]
    must be exactly length 2. The point represents a valid node on the board,
    and the second element is a valid move direction. [Start] must be connected
    to a valid start node.*)
type command =
  | DrawLine of phrase
  | StartNode of Flow.node
  | Start
  | Undo
  | Clear
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Rasied when a malformed command is parsed. *)

val make_tuple : string -> int * int
val get_move_type : string -> State.move_type

val parse : string -> command
(** [parse str] parses a player's input into a [command]. The word sequence
    before the first "(" is the action. The rest of the command is the phrase.
    Examples:

    - [parse "R"] is [DrawLine R]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only characters: A-Z, a-z, 0-9, (, ), and spaces.

    Raises: [Empty] if [str] is the empty string.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the phrase is not L | R | U | D, if the action is anything but "quit", "draw
    line", "start", or "clear". *)
