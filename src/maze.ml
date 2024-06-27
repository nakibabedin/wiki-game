open! Core

type direction =
  | Left
  | Right
  | Up
  | Down

type pointType =
  | Start
  | Path
  | Wall
  | End
[@@deriving equal]

module Point = struct
  type t =
    { row : int
    ; col : int
    ; value : pointType
    ; mutable visited : bool
    }

  let create row_val col_val point_type =
    { row = row_val; col = col_val; value = point_type; visited = false }
  ;;
end

type current_point =
  { mutable row : int
  ; mutable col : int
  }

let generate_list_of_points input_file =
  let maze_list = In_channel.read_lines (File_path.to_string input_file) in
  let maze_grid = List.map maze_list ~f:(fun line -> String.to_list line) in
  let curr = { row = 0; col = 0 } in
  let parsed_graph =
    List.concat_map maze_grid ~f:(fun maze_line ->
      curr.col <- 0;
      let parsed_line =
        List.map maze_line ~f:(fun maze_point ->
          curr.col <- curr.col + 1;
          match maze_point with
          | 'S' -> Point.create curr.row (curr.col - 1) Start
          | 'E' -> Point.create curr.row (curr.col - 1) End
          | '.' -> Point.create curr.row (curr.col - 1) Path
          | _ -> Point.create curr.row (curr.col - 1) Wall)
      in
      curr.row <- curr.row + 1;
      parsed_line)
  in
  parsed_graph
;;

let start_point (list_of_points : Point.t list) =
  let result =
    List.filter list_of_points ~f:(fun point ->
      match point.value with Start -> true | _ -> false)
  in
  match result with
  | [] -> failwith "No Starting Point Detected"
  | h :: _ -> h
;;

let is_valid_point (input_point : Point.t) (list_of_points : Point.t list) =
  let point_in_list =
    List.filter list_of_points ~f:(fun list_point ->
      equal list_point.row input_point.row
      && equal list_point.col input_point.col)
  in
  match point_in_list with
  | [] -> failwith "point doesn't exist, this shouldn't happen"
  | h :: _ -> (match h.value with Wall -> false | _ -> not h.visited)
;;

(* let is_end_point (input_point : Point.t) (list_of_points : Point.t list) =
   let point_in_list = List.filter list_of_points ~f:(fun list_point -> equal
   list_point.row input_point.row && equal list_point.col input_point.col) in
   match point_in_list with | [] -> failwith "point doesn't exist, this
   shouldn't happen" | h :: _ -> (match h.value with End -> true | _ ->
   false) ;; *)

(* From a point, move in a certain direction *)
(* return an option with Some point or None if out of bounds *)
let move_direction
  (input_point : Point.t)
  (direction : direction)
  (list_of_points : Point.t list)
  =
  match direction with
  | Left ->
    let point =
      List.filter list_of_points ~f:(fun list_point ->
        equal list_point.row (input_point.row - 1)
        && equal list_point.col input_point.col)
    in
    (match point with
     | [] -> None
     | h :: _ ->
       (match is_valid_point h list_of_points with
        | false -> None
        | true -> Some h))
  | Right ->
    let point =
      List.filter list_of_points ~f:(fun list_point ->
        equal list_point.row (input_point.row + 1)
        && equal list_point.col input_point.col)
    in
    (match point with
     | [] -> None
     | h :: _ ->
       (match is_valid_point h list_of_points with
        | false -> None
        | true -> Some h))
  | Up ->
    let point =
      List.filter list_of_points ~f:(fun list_point ->
        equal list_point.row input_point.row
        && equal list_point.col (input_point.col - 1))
    in
    (match point with
     | [] -> None
     | h :: _ ->
       (match is_valid_point h list_of_points with
        | false -> None
        | true -> Some h))
  | Down ->
    let point =
      List.filter list_of_points ~f:(fun list_point ->
        equal list_point.row input_point.row
        && equal list_point.col (input_point.col + 1))
    in
    (match point with
     | [] -> None
     | h :: _ ->
       (match is_valid_point h list_of_points with
        | false -> None
        | true -> Some h))
;;

let get_move_direction (source : Point.t) (dest : Point.t) =
  match dest.row - source.row, dest.col - source.col with
  | 0, -1 -> Left
  | 0, 1 -> Right
  | 1, 0 -> Down
  | -1, 0 -> Up
  | _ -> failwith "Not a valid move, this shouldnt happen"
;;

let rec pathfinder (node : Point.t) (path : direction list) graph =
  node.visited <- true;
  match node.value with
  | Wall -> failwith "Ran into a wall, this shouldn't happen"
  | End -> Some path
  | Path | Start ->
    let directions = [ Left; Right; Up; Down ] in
    let valid_next_moves_opt =
      List.map directions ~f:(fun direction ->
        move_direction node direction graph)
    in
    let valid_next_moves = List.filter_opt valid_next_moves_opt in
    (match valid_next_moves with
     | [] -> None
     | _ ->
       List.find_map valid_next_moves ~f:(fun next_node ->
         pathfinder
           next_node
           (List.append path [ get_move_direction node next_node ])
           graph))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze = generate_list_of_points input_file in
        (* let starting_point = start_point maze in *)
        let start = start_point maze in
        let path_opt = pathfinder start [] maze in
        match path_opt with
        | None -> failwith "No path in maze"
        | Some path ->
          List.iter path ~f:(fun direction ->
            match direction with
            | Left -> print_endline "Left"
            | Right -> print_endline "Right"
            | Up -> print_endline "Up"
            | Down -> print_endline "Down")]
;;

(* ignore (input_file : File_path.t); failwith "TODO"] *)

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
