open OUnit2
open Project
open Flow
open Parsing
open Command
open State
open Graphics
open Printer
open StateTest
open ParsingTest

(***************************************************************************)

(** Test plan: This file will be testing functions in our [State] and [Parsing]
    module, which advance the state of the current flow game as the user plays
    and makes moves and parsing from json to our board type.

    Those are the only modules in our game that can be automatically tested. The
    test is generated by:

    - Black box testing: For simple cases
    - Glass box testing: For raising exception cases
    - Random: For large/ complicated cases

    The rest of the game (UI part) is manually tested for bugs as it is a
    Graphics implementation.

    Testing state is crucial to proving our game correct, as state changes all
    of the knowledge about which pipes are completed and which are not. If
    [State] and [Parsing] is not correct, then our game cannot be correct. *)

(***************************************************************************)

let suite =
  "test suite for final project"
  >::: List.flatten [ state_test_suit; parsing_test_suit ]

let _ = run_test_tt_main suite
