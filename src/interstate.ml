open! Core
module City = String
module Highway = String

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G = Graph.Imperative.Graph.Concrete (City)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* We separate out the [Network] module to represent our highway network in
   OCaml types. *)
(* module Network = struct (* Highway --> List of Cities *) module Highway =
   struct type t = {cities: string list} end *)

module Network = struct
  module Connection = struct
    module T = struct
      type t = Highway.t * (City.t * City.t) [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    let create (highway_name : string) (source : string) (dest : string) =
      ( Highway.of_string highway_name
      , (City.of_string source, City.of_string dest) )
    ;;

    let _of_string s =
      (* Given a line from the input file, creates pairing with highway and
         all the cities inside of it *)
      let highway_connections = String.split s ~on:',' in
      match highway_connections with
      | [] -> None
      | h :: t -> Some (Highway.of_string h, t)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  (* Given a list and highway_name, generates all connections *)
  (* Current_pairs is to aid with the storage of these arguments through
     recursive calls *)
  let rec generate_pairs lst highway_name =
    match lst with
    | [] -> None
    | h :: t ->
      Some
        (let connections =
           List.map t ~f:(fun dest ->
             [ Connection.create
                 highway_name
                 (Highway.of_string h)
                 (Highway.of_string dest)
             ; Connection.create
                 highway_name
                 (Highway.of_string dest)
                 (Highway.of_string h)
             ])
         in
         let other_connections = generate_pairs t highway_name in
         match other_connections with
         | None -> connections
         | Some new_connections -> connections @ new_connections)
  ;;

  let of_file input_file =
    (* Read in all the lines as a list of lines *)
    let lines = In_channel.read_lines (File_path.to_string input_file) in
    let final_connections =
      List.concat_map lines ~f:(fun line ->
        let highway_connections = Connection._of_string line in
        match highway_connections with
        | None -> failwith "highway not found"
        | Some highway ->
          let highway_name, connections_list = highway in
          (match generate_pairs connections_list highway_name with
           | None -> []
           | Some pairs -> List.concat pairs))
    in
    Connection.Set.of_list final_connections
  ;;

  (* End of Day *)
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (_highway, (source, dest)) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph source dest);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
