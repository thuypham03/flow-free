open Yojson.Basic.Util
open Flow

let dummy_node : node = (-1, -1)

let node_from_json json =
  match json |> to_list |> List.map to_int with
  | [ x; y ] -> (x, y)
  | _ -> dummy_node

let start_end_from_json json =
  match json |> to_list |> List.map node_from_json with
  | [ x; y ] -> (x, y)
  | _ -> (dummy_node, dummy_node)

let color_from_json json =
  match json |> to_list |> List.map to_int with
  | [ r; g; b ] -> (r, g, b)
  | _ -> (0, 0, 0)

let pipe_from_json json =
  {
    start_end = json |> member "start_end" |> start_end_from_json;
    color = json |> member "color" |> color_from_json;
    path = json |> member "path" |> to_list |> List.map node_from_json;
  }

let board_from_json json =
  {
    size = json |> member "size" |> to_int;
    pipe_list = json |> member "pipe_list" |> to_list |> List.map pipe_from_json;
    occupied = json |> member "occupied" |> to_list |> List.map node_from_json;
    current_pipe = json |> member "current_pipe" |> pipe_from_json;
    current_node = json |> member "current_node" |> node_from_json;
  }
