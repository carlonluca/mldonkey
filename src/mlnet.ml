(* main.ml *)

open DriverMain

let () =
  print_endline "Hello, world!";
  main_app;
  print_endline "Press Enter to exit...";
  ignore (read_line ())
