open! Core
open! Expect_test_helpers_core
open! Wiki_game_lib
(* let%expect_test "get_first_item_of_all_unordered_lists" = (* This test
   uses existing files on the filesystem. *) let contents =
   File_fetcher.fetch_exn (Local (File_path.of_string "../resources/wiki"))
   ~resource:"Carnivore" in List.iter (get_first_item_of_all_unordered_lists
   contents) ~f:print_endline; [%expect {| All feliforms, such as domestic
   cats, big cats, hyenas, mongooses, civets All birds of prey, such as
   hawks, eagles, falcons and owls All vultures, both old world and new Most
   waterfowl, such as gulls, penguins, pelicans, storks, and herons |}] ;; *)
