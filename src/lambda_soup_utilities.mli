open! Core

(** [Lambda_soup_utilities] exposes some simple commands that use the Lambda Soup library
    (https://github.com/aantron/lambdasoup) to parse HTML. *)

val command : Command.t
val get_first_item_of_all_unordered_lists : string -> string list
val get_first_item_of_second_unordered_list : string -> string
