(* main.ml *)

open DriverMain
open CommonOptions
open CommonBlocking
open Printf2

let rec loop () =
  print_endline "Tick";
  Unix.sleep 1;
  loop ()

let () =
  print_endline "Hello, world!";
  CommonNetwork.init ();
  Printf2.init ();
  DriverMain.init ();
  Misc.init ();
  BasicSocket.init ();
  DonkeyMain.init ();
  print_endline "Press Enter to exit...";
  loop ()