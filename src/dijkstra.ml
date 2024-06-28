(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
[@@@disable_unused_warnings]

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it later for
      looking up and setting the state of nodes in the course of our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)

  let a t = t.a
  let b t = t.b
  let distance t = t.distance
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors t (node_id : Node_id.t) : (Node_id.t * int) list =
    let relevant_edges =
      List.filter t ~f:(fun edge ->
        Node_id.equal node_id (Edge.a edge)
        || Node_id.equal node_id (Edge.b edge))
    in
    List.map relevant_edges ~f:(fun edge ->
      match Node_id.equal (Edge.a edge) node_id with
      | true -> Edge.b edge, abs (Edge.distance edge)
      | false -> Edge.a edge, abs (Edge.distance edge))
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test "neighbors" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module T = struct
    module State = struct
      type t =
        | Origin (** Used to mark the node where our search starts *)
        | Unseen (** Used to mark unexplored Nodes *)
        | Todo of
            { distance : int
            ; via : Node_id.t
            }
        (** Used to mark nodes that have been encountered but have not been processed yet *)
        | Done of { via : Node_id.t }
        (** Used to mark nodes that we are finished processing *)
      [@@deriving sexp, compare]
    end

    include State

    type t = { mutable state : State.t }
    [@@deriving fields ~getters, sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let init () = { state = Unseen }
  let set_state t state = t.state <- state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These [Node.t]s
      will be updated in the course of our graph search to keep track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges (edges : Edges.t) =
    List.fold
      edges
      ~init:(Map.empty (module Node_id))
      ~f:(fun map (edge : Edge.t) ->
        let node_a = Node.init () in
        let node_b = Node.init () in
        let new_map = Map.add_exn map ~key:(Edge.a edge) ~data:node_a in
        Map.add_exn new_map ~key:(Edge.a edge) ~data:node_a)
  ;;

  let find = Map.find_exn
  let state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  type min =
    { mutable next_node_to_visit : Node_id.t
    ; mutable distance : int
    ; mutable via_route : Node_id.t
    }

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)
  let next_node t : (Node_id.t * (int * Node_id.t)) option =
    let min_val =
      { next_node_to_visit = Node_id.create 0
      ; distance = Int.max_value
      ; via_route = Node_id.create 0
      }
    in
    Map.iteri t ~f:(fun ~key:id ~data:node ->
      match Node.state node with
      | Origin | Done _ | Unseen -> ()
      | Todo data ->
        (match min_val.distance > data.distance with
         | true ->
           min_val.distance <- data.distance;
           min_val.via_route <- data.via;
           min_val.next_node_to_visit <- id
         | false -> ()));
    let no_next_move = equal min_val.distance Int.max_value in
    match no_next_move with
    | true -> None
    | false ->
      Some (min_val.next_node_to_visit, (min_val.distance, min_val.via_route))
  ;;

  (* let%expect_test ("next_node" [@tags "disabled"]) = *)
  let%expect_test "next_node" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  (* queue contains the path to the destination so far let rec traverse t
     queue curr_node destination = match Node_id.equal curr_node destination
     with | true -> queue | false -> ( Queue.enqueue queue curr_node;
     next_node_to_visit = next_node t

     ) *)

  (* goes backwards till we find the origin *)
  let rec traverse max map_of_nodes curr_node origin path_of_nodes =
    match equal max 10 with
    | true -> []
    | false ->
      (match Node_id.equal curr_node origin with
       | true -> List.append [ origin ] path_of_nodes
       | false ->
         let first_node_map =
           Map.filteri map_of_nodes ~f:(fun ~key:id ~data:_ ->
             Node_id.equal id curr_node)
         in
         let first_node = List.hd_exn (Map.to_alist first_node_map) in
         let prev_node = Node.state (snd first_node) in
         (match prev_node with
          | Done prev_node_id ->
            traverse
              (max + 1)
              map_of_nodes
              prev_node_id.via
              origin
              ([ curr_node ] @ path_of_nodes)
          | _ -> []))
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [distination]. *)
  let path t destination : Node_id.t list =
    (* let _path_to_dest = Queue.create () in *)
    let shortest_path_nodes =
      Map.filteri t ~f:(fun ~key:id ~data:node ->
        match Node.state node with Origin | Done _ -> true | _ -> false)
    in
    let shortest_path = [] in
    let start_of_path =
      Map.filteri t ~f:(fun ~key:id ~data:node ->
        match Node.state node with Origin -> true | _ -> false)
    in
    let start_node = List.hd_exn (Map.to_alist start_of_path) in
    traverse 0 shortest_path_nodes destination (fst start_node) []
  ;;

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test "path" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let path_ans = path t n2 in
    print_s [%message (path_ans : Node_id.t list)];
    [%expect {|
     (path_ans (0 1 2)) |}]
  ;;
end

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let shortest_path ~edges ~origin ~destination : Node_id.t list = []

let%expect_test ("shortest_path" [@tags "disabled"]) =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
