open OUnit2
open Project
open Flow
open Parsing
open Command
open State
open Graphics

(** Helper functions for testing the four main [State] functions. *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_node (n : node) : string =
  "(" ^ string_of_int (fst n) ^ ", " ^ string_of_int (snd n) ^ ")"

let pp_pipe_path (p : pipe) : string = pp_list pp_node p.path

let pp_board_occupied (b : board) : string = pp_list pp_node b.occupied
