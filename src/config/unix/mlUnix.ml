(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf2
open Unix

let execvp_command cmd args handler = 
  let (in_read, output) = Unix.pipe() in
  let (input, out_write) = Unix.pipe() in
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork () with
            0 -> begin
                try
                  if input <> Unix.stdin then
                    begin Unix.dup2 input Unix.stdin; Unix.close input end;
                  if output <> Unix.stdout then
                    begin Unix.dup2 output Unix.stdout; Unix.close output end;
                  Unix.close in_read;
                  Unix.close out_write;
                  Unix.execvp cmd args;
                  exit 127
                with e -> 
                    Printf.eprintf "Exception %s in exec_command\n"
                      (Printexc2.to_string e) ; 
                    exit 1
              end
          | id -> 
              exit 2
        with _ -> 
            exit 3
      end
  | id -> 
      ignore (snd(Unix.waitpid [] id));
      Unix.close input;
      Unix.close output;
      let sock = handler in_read out_write in
      sock, id

let fork_and_exec cmd args = 
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork() with
            0 -> begin
                try
                  Unix.execv cmd args;
                  exit 0
                with e -> 
                    lprintf "Exception %s while starting file_completed_cmd\n" (Printexc2.to_string e); 
                    exit 127
              end
          | id -> exit 0
        with _ -> exit 0
      end
  | id -> ignore (snd(Unix.waitpid [] id))
      
let setuid = Unix.setuid
let set_close_on_exec = Unix.set_close_on_exec
let set_signal signal f = Sys.set_signal signal f
  
  
external getdtablesize : unit -> int = "ml_getdtablesize"
  
let max_all_sockets = getdtablesize ()
let max_sockets = max (max_all_sockets - 100) (max_all_sockets / 2)
let max_filedescs = (max_all_sockets - max_sockets) / 2

let chroot = Unix.chroot  


let write = Unix.write
  
let detach_daemon () =
  try
    let pid =  Unix.fork () in
    if pid < 0 then failwith "Error in fork";
    if pid > 0 then exit 0;
    let sid = Unix.setsid () in

(* Changed in 2.5.27 *)
    begin
      let stdin_new = Unix.openfile "/dev/null" [O_RDONLY] 0o444 in
      Unix.dup2 stdin_new Unix.stdin;
      Unix.close stdin_new;
    end;
    
    begin
      let stdout_new = Unix.openfile "/dev/null" [O_WRONLY] 0o666 in
      Unix.dup2 stdout_new Unix.stdout;
      Unix.dup2 stdout_new Unix.stderr;
      Unix.close stdout_new;
    end;
    
    Printf2.detach ();
        
  with e ->
      lprintf "Exception %s in detach_daemon\n"
        (Printexc2.to_string e); 
      exit 2

let set_nonblock = Unix.set_nonblock
