open! Core

module Article = struct

  type t = 
  {
    name:String.t; url:String.t 
  } [@@deriving compare, sexp, hash, equal]

  let create ~url = 
    let url_parts = String.split url ~on:'/' in
    let name_opt = List.last url_parts in
    match name_opt with
    | None -> failwith "No name detected"
    | Some name ->  { name ; url}
  
    (* let to_string t = t.name; *)
  

end

module Connection = struct
  
  module T = struct
  (* Article -> List of connected articles *)
    type t = (Article.t * Article.t) [@@deriving compare, sexp, hash, equal]
  end

  include Comparable.Make(T)
  
end


module G = Graph.Imperative.Graph.Concrete (String)

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
    let vertex_name v = sprintf !"\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)


let not_namespace link =
  let option = (Wikipedia_namespace.namespace link) in
  match option with
  | None -> true
  | Some _namespace -> false;

;;

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  

  let open Soup in
  parse contents
  $$ "a[href*=/wiki/]" 
(* in R.attribute "href" contents *)
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:( not_namespace )
  |> List.dedup_and_sort ~compare:(String.compare)


  (* ignore (contents : string);
  failwith "TODO" *)
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki/"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {| 
    /wiki/Carnivore
    /wiki/Domestication_of_the_cat
    /wiki/Mammal
    /wiki/Species

|}]
;;


let correct_url url (how_to_fetch :File_fetcher.How_to_fetch.t) =
  match how_to_fetch with
  | Local _ -> url
  | Remote -> if not (String.is_prefix ~prefix:"https://" url) then "https://en.wikipedia.org" ^ url else url


let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* returns a list of all of the connections that our graph would have *)
let rec visualize_helper max_depth ~(origin:String.t)  ~how_to_fetch =

  (* Setup the Graph from the current node *)

  (* let graph = G.create () in *)

  let origin_contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let adjacent_nodes = get_linked_articles origin_contents in
  let edges = List.map adjacent_nodes ~f:(
    fun edge_node -> (
      let article_edge = Article.create ~url:edge_node in
      let article_origin = Article.create ~url:origin in
      (article_origin, article_edge)
    )
  ) in

  match max_depth > 0 with
  | false -> edges
  | true -> (
      let subgraph_connection_lists = List.concat_map adjacent_nodes ~f:(
        fun node -> 
          (* graph starting from node *)
           visualize_helper (max_depth-1) ~origin:node ~how_to_fetch
      ) in
      edges @ subgraph_connection_lists
  )


(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =

  let list_of_connections = visualize_helper max_depth ~origin ~how_to_fetch in
  let graph = G.create () in
  List.iter list_of_connections ~f:( 
    fun connection -> 
      let src, dest = connection in
      G.add_edge graph src.name dest.name
    );

  Dot.output_graph (Out_channel.create (File_path.to_string output_file)) graph;
  printf !"Done! Wrote dot file to %{File_path}\n%!" output_file;


  (* ignore (max_depth : int);
  ignore (origin : string);
  ignore (output_file : File_path.t);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO" *)
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;


(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)


(* let rec find_path_helper ?(max_depth = 3) ~(origin:String.t) ~(destination:String.t) ~how_to_fetch ~visited ()=

  print_endline origin;

  let correct_origin = correct_url origin how_to_fetch in
  
  match List.exists visited ~f:(String.equal correct_origin) with
  | true -> None
  | false -> (

  (* Check if we're at destination *)
  match (String.equal correct_origin destination) with 
  | true -> Some [destination]
  | false -> (
    (* We aren't at destination, now check if we can search any farther *)
      match (equal max_depth 0) with
      (* if true, then there is no path from here onwards *)
      | true -> None
      | false -> ( 
        (* try to search further down *)
        let origin_contents = File_fetcher.fetch_exn how_to_fetch ~resource:correct_origin in
        let adjacent_nodes = get_linked_articles origin_contents in 
        let rest_of_path_opt = List.find_map adjacent_nodes ~f:(
          fun next_node -> find_path_helper ~max_depth:(max_depth-1) ~origin:next_node ~destination ~how_to_fetch ~visited:(List.append visited [correct_origin]) ()
        ) in 

        match rest_of_path_opt with 
        | None -> None
        | Some rest_of_path -> Some (List.append [correct_origin] rest_of_path)
      )))


  (* ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO" *)
;; *)

(* Given a queue and destination, returns true if destination is in queue*)
let check_for_dest ~destination ~(level:(string * string list) Base.Queue.t) ~how_to_fetch= 
  let level_list = Queue.to_list level in 
  List.exists level_list ~f:(fun list_item ->
    
    let first_item, _ = list_item in 
    (* printf "first_item: %s\n"first_item; *)
    (* printf "destination: %s\n"destination; *)
    (* print_endline " "; *)
    String.equal (correct_url destination how_to_fetch) (correct_url first_item how_to_fetch) )

;;

(* to help with level order traversal *)
let rec find_path_helper ?(max_depth=3) ~(level:(string * string list) Base.Queue.t) ~(destination:String.t) ~how_to_fetch ~visited() =


  let final_level = check_for_dest ~destination ~level ~how_to_fetch in
  match final_level with 
  | true -> Some ( 
    let (dest_opt) = Queue.find level ~f:(
      fun final_item -> 
        let node, _ = final_item in 
        String.equal node destination) in 

        match dest_opt with 
        | None -> failwith "path not found at all -- Shouldn't happen"
        | Some dest -> snd dest
        
        )
  | false -> (
        match equal max_depth 0 with 
        | true -> None
        | false -> (


    let next_level :(string * string list) Base.Queue.t = Queue.create () in
    let next_visited = List.append visited (Queue.to_list level) in
    Queue.iter level ~f:(
      
      fun level_node ->  
        let curr_node, curr_path = level_node in
        let correct_origin = correct_url curr_node how_to_fetch in 

        (* print_endline correct_origin; *)
        
        let origin_contents = File_fetcher.fetch_exn how_to_fetch ~resource:correct_origin in

        let adjacent_nodes = get_linked_articles origin_contents in 
        List.iter adjacent_nodes ~f:(fun next_node -> Queue.enqueue next_level (correct_url next_node how_to_fetch , (curr_path @ [next_node]))););

      let next_level_without_dupes = Queue.filter next_level ~f:(fun node -> not (List.exists visited ~f:(
        fun item -> String.equal (fst item) (fst node)
      ))) in
    
      find_path_helper ~max_depth:(max_depth-1) ~level:next_level_without_dupes ~destination ~how_to_fetch ~visited:next_visited) ())
;



;;


(* let rec find_path_helper ?(max_depth=3) ~(origin:String.t) ~(destination:String.t) ~how_to_fetch ~visited ~path () =

  let correct_origin = correct_url origin how_to_fetch in 

  match List.exists visited ~f:(String.equal correct_origin) with
  | true -> None
  | false -> (
    (* Check if we're at destination *)
      match (String.equal correct_origin destination) with 
    | true -> Some [destination]
    | false -> (
      (* try to search further down *)
      let origin_contents = File_fetcher.fetch_exn how_to_fetch ~resource:correct_origin in
      let adjacent_nodes = get_linked_articles origin_contents in 
      let level_order_traversal = Queue.of_list adjacent_nodes in
      match check_for_dest ~destination ~level:level_order_traversal with 
      | true -> Some (path @ [destination])
      | false -> 


        )) *)


;;


let find_path ?(max_depth = 2) ~(origin:String.t) ~(destination:String.t) ~(how_to_fetch:File_fetcher.How_to_fetch.t) () =
  let visited = [] in
  let first_level =  Queue.create () in
  let new_origin =  correct_url origin how_to_fetch in
  let new_destination = correct_url destination how_to_fetch in
  Queue.enqueue first_level ((new_origin), [new_origin]);

  find_path_helper ~max_depth ~level:first_level ~destination:new_destination ~how_to_fetch ~visited ()
  
  (* match how_to_fetch with
  | Local _ -> find_path_helper ~max_depth ~origin ~destination ~how_to_fetch ~visited ()
  | Remote -> (
    let new_origin = "https://en.wikipedia.org/" @ origin in
    find_path_helper ~max_depth ~new_origin ~destination ~how_to_fetch ~visited ()
    ) *)
  
  ;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:(fun item -> print_endline (correct_url item how_to_fetch))]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
