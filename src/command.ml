(* Parsing implementation from console. *)

type phrase = State.move_type

type command =
  | DrawLine of phrase
  | StartNode of Flow.node
  | Start
  | Undo
  | Clear
  | Quit

exception Empty
exception Malformed

let make_tuple str =
  let tuple_lst =
    str |> String.split_on_char ' ' |> String.concat ""
    |> String.split_on_char '(' |> String.concat "" |> String.split_on_char ')'
    |> String.concat "" |> String.split_on_char ','
    |> List.filter (fun a ->
           if a <> "(" || a <> ")" || a <> "," then true else false)
  in
  match tuple_lst with
  | [ h; t ] -> (int_of_string h, int_of_string t)
  | _ -> raise Malformed

let get_move_type (str : string) : State.move_type =
  if str = "L" then L
  else if str = "R" then R
  else if str = "D" then D
  else if str = "U" then U
  else raise Malformed

let parse str =
  let str_lst =
    List.filter
      (fun a -> if a <> "" then true else false)
      (String.split_on_char ' ' str)
  in
  match str_lst with
  | [] -> raise Empty
  | [ "quit" ] -> Quit
  | [ "clear" ] -> Clear
  | [ "start" ] -> Start
  | [ "undo" ] -> Undo
  | h :: t when h = "start" -> StartNode (make_tuple (String.concat "" t))
  | [ h ] -> DrawLine (get_move_type h)
  | _ -> raise Malformed
