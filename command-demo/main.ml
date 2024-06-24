open! Core

(* In OCaml, we use the [Command] library to write apps that take commandline arguments.

   You should understand generally how to construct apps that use [Command], although you
   are allowed to treat this roughly as a magic incantation template that you can
   copy-paste every time you need it in your own app.

   Here, we'll demonstrate a basic usage of [Command] and the type of commandline
   arguments we can create using it.

   You can run this program with
   {[
     $ cd snake/command-demo
     $ dune exec ./main.exe -- -help
   ]}

   The "dune exec" invocation should be familiar as the way that you ran the snake
   game. Afterwards, we need the double hyphen "--" to pass any other arguments into the
   main.exe binary. (You can accept this as an incantation for passing arguments when
   running OCaml binaries using "dune exec", but ask a TA if you want to know more.)

   The command above will show you some help related to the arguments you need to pass to
   run the program. Try this:

   {[
     $ dune exec ./main.exe -- -name "Alice"
   ]}

   Now it should actually run, and print something to stdout.

   Take a look at the following code, and try to use the other two supported commandline
   arguments as well. *)

let command =
  Command.basic
    ~summary:"A hello world program using the Command module"
    (* This "%map_open.Command" does two things:

       1. It's calling [Command.map] to parse the command line args.

       2. It makes [Command.Param] available (in scope) when you define the args.

       Again, it's not important to fully understand the mechanics here, and to accept
       this as standard syntax for defining commandline commands. *)
    [%map_open.Command
      (* This first item [ let () = return () ] does nothing. We've chosen to keep it
          here in the example because it's easy to copy-paste and shows you the syntax
          for a "command with zero arguments". If you hate it, you can remove it. *)
      let ()     = return ()
      (* Here are a few examples of flags that can be defined with [Command]. You can
         check out https://ocaml.org/p/core/v0.15.0/doc/Core/Command/Flag/index.html for
         documentation on other bells and whistles that are available.  *)
      and name   = flag "-name" (required string) ~doc:"STRING e.g., Brett"
      and number = flag "-number" (optional int) ~doc:"INT pick a number"
      and say_goodbye = flag "-say-goodbye" no_arg ~doc:"say goodbye too" in
      (* Once you get down here, [Command.Param] is no longer in scope.

         Thanks to [Command.map], we're now in a context where all three params have been
         neatly parsed. We can use them below like ordinary OCaml values. *)
      fun () ->
        printf "Hey there, %s!\n" name;
        Option.iter number ~f:(printf "Your number is %d\n");
        if say_goodbye then printf "Goodbye!"]
;;

let () = Command_unix.run command
