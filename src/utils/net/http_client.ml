(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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


(* HTTP Requests:
  GET, POST, HEAD, PUT, DELETE, TRACE, OPTIONS, CONNECT
*)

open Printf2
open BasicSocket
open Unix
open Url
open TcpBufferedSocket


type http_request =
  GET
| POST
| HEAD
| PUT
| DELETE
| TRACE

let verbose = ref false

  (*
| OPTIONS of url option (* None = '*' *)
| CONNECT of string * int
*)

type request = {
    req_headers : ( string * string ) list;
    req_user_agent : string;
    req_accept : string;
    req_proxy : (string * int) option;

    req_url : url;
    req_request : http_request;
    req_referer : Url.url option;
  }

type content_handler = 
  int -> (string * string) list -> TcpBufferedSocket.t -> int -> unit

let basic_request = {
    req_url = Url.of_string "http://www.mldonkey.net/";
    req_referer = None;

    req_request = GET;
    req_proxy = None;
    req_headers = [];
    req_user_agent = "Wget 1.4";
    req_accept = "*/*";
  }
      
let make_full_request r =
  let url = r.req_url in
  let args = url.args in
  let res = Buffer.create 80 in
  let is_real_post = r.req_request = POST && args <> [] in
  if is_real_post
  then Buffer.add_string res "POST "
  else 
    Buffer.add_string res (if r.req_request = HEAD then "HEAD " else "GET ");
  Buffer.add_string res (
    let url = 
      if r.req_proxy <> None
      then  Url.to_string_no_args url
      else url.short_file
    in
	(* I get a lot more bittorrent urls with this line: *)
	let url = (Str.global_replace (Str.regexp " ") "%20" url) in
    let url = if is_real_post then url else
        Url.put_args url args
    in
    url);
  Printf.bprintf res " HTTP/1.0\r\nHost: %s%s\r\n" url.server (if url.port != 80 then Printf.sprintf ":%d" url.port else "");
  List.iter (fun (a,b) ->
      Printf.bprintf res "%s: %s\r\n" a b
  ) r.req_headers;
  Printf.bprintf res "User-Agent: %s\r\n" r.req_user_agent;
  Printf.bprintf res "Accept: %s\r\n" r.req_accept;
  Printf.bprintf res "Connection: close\r\n";
 (match r.req_referer with None -> ()
    | Some url -> 
        Printf.bprintf res "Referer: %s\r\n" (Url.to_string_no_args url));
  if is_real_post then begin
      let post = Buffer.create 80 in
      let rec make_post = function
          | [] -> assert false
        | [a, b] ->
            Printf.bprintf post "%s%c%s" (Url.encode a) '=' (Url.encode b)
        | (a,b)::l ->
            Printf.bprintf post "%s%c%s%c" 
              (Url.encode a) '=' (Url.encode b) '&';
            make_post l in
      make_post args;
      Printf.bprintf res "Content-Type: application/x-www-form-urlencoded\r\nContent-Length: %d\r\n\r\n%s"
        (Buffer.length post) (Buffer.contents post)
    end else
    Buffer.add_string res "\r\n";
  let s = Buffer.contents res in
  if !verbose then
    lprintf "Http_client.make_full_request on URL: %s\n" s;
  s

let split_head s =
  let rec iter pos1 res =
    try
      let pos3 = String.index_from s pos1 '\n' in
      let pos2 = if pos3 > 0 && s.[pos3 - 1] = '\r' then pos3 - 1 else pos3 in
      let line = String.sub s pos1 (pos2 - pos1) in
      if line = "" then List.rev res else
      iter (pos3+1) (line :: res)
    with _ -> 
        let last_line = String.sub s pos1 (String.length s - pos1) in
        List.rev (if last_line = "" then res else last_line :: res)
  in
  iter 0 []

let parse_header headers_handler sock header =
  let headers = split_head header in
  match headers with 
    [] -> failwith "Ill formed reply"
  | ans :: headers ->
      if !verbose then lprintf "Http_client.parse_header: ANSWER %s\n" ans;
      let ans_code = int_of_string (String.sub ans 9 3) in
      let headers = List.map (fun s ->
            let sep = String.index s ':' in
            (* TODO: we should lowercase the names here!
               The header-names are case-insensitive,
               therefore we only use lowercased names. *)
            let name_head = String.sub s 0 sep in
            let size = String.length s in
            let content_head = String.sub s (sep+2) (size-sep-2) in
            (name_head, content_head)
        ) headers in
      try
        headers_handler sock ans_code headers;
      with _ -> 
          TcpBufferedSocket.close sock (Closed_for_error "bad header")

let read_header header_handler sock nread =  
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let new_pos = maxi 0 (new_pos - 1) in
  (*
  lprintf "received [%s]" (String.escaped
      (String.sub b.buf new_pos nread));
  *)
  let rec iter i =
    let end_pos = b.pos + b.len in
    if i < end_pos then
      if b.buf.[i] = '\n' && i <= end_pos - 2 then
        let c = b.buf.[i+1] in
        if c = '\n' then
          let len = i + 2 - b.pos in
          let header = String.sub b.buf b.pos len in
          buf_used b len;
          header_handler sock header
        else
        if c = '\r' && i <= end_pos - 3 && b.buf.[i+2] = '\n' then
          let len = i + 3 - b.pos in
          let header = String.sub b.buf b.pos len in
          buf_used b len;
          header_handler sock header
        else 
          iter (i+1)
      else
        iter (i+1)
    else
      ()
  in
  iter new_pos

let http_reply_handler nr headers_handler sock nread =
(*  lprintf "http_reply_handler\n"; *)
  nr := true;
  read_header (parse_header headers_handler) sock nread
  
let get_page r content_handler f =
  let error = ref false in
  let rec get_url level r =
  try
    let url = r.req_url in
(*
    let args = ref [] in
    let headers = ref [] in
    let ispost = ref false in
    let timeout = ref 300.0 in
    let proxy = ref None in
    List.iter (function
      | Args l -> args := l@ !args
      | Headers l -> headers := l @ !headers;
      | Post -> ispost := true
      | Proxy (h,p) -> proxy := Some (h,p)
    ) get_args;
    let args = !args in
    let headers = !headers in
    let ispost = !ispost in
    let proxy = !proxy in
*)    
    let request = make_full_request r in
    
    
    let server, port =
      match r.req_proxy with
      | None -> url.server, url.port
      | Some (s, p) -> s, p
    in
(*    lprintf "async_ip ...\n"; *)
    Ip.async_ip server (fun ip ->
(*         lprintf "IP done %s:%d\n" (Ip.to_string ip) port;*)
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.connect token "http client connecting"
            (try Ip.to_inet_addr ip with e -> raise Not_found)
          port (fun sock e -> 
              () 
(*              if !verbose then
                  lprintf "Event %s\n"
                    (match e with
                      CONNECTED -> "CONNECTED"
                    | WRITE_DONE -> "WRITE_DONE"
                    | CAN_REFILL -> "CAN_REFILL"
                    | BUFFER_OVERFLOW -> "BUFFER_OVERFLOW"
                    | READ_DONE n -> Printf.sprintf "READ_DONE %d" n
                    | BASIC_EVENT e ->
                        match e with
                          (CLOSED s) -> Printf.sprintf "CLOSED %s" (string_of_reason s)
                        | RTIMEOUT -> "RTIMEOUT"
                        | LTIMEOUT -> "LTIMEOUT"
                        | WTIMEOUT -> "WTIMEOUT"
                        | CAN_READ -> "CAN_READ"
                        | CAN_WRITE -> "CAN_WRITE"
                    )
 *)
          )
        in
        let nread = ref false in
        if !verbose then 
          lprintf "Http_client.get_page: %s\n" (String.escaped request);
        TcpBufferedSocket.write_string sock request;
        TcpBufferedSocket.set_reader sock (http_reply_handler nread
            (default_headers_handler url level));
        set_rtimeout sock 5.;
        TcpBufferedSocket.set_closer sock (fun _ _ -> ()
(*        lprintf "Connection closed nread:%b\n" !nread; *)
        )

    )
  with e -> lprintf "Http_client: error in get_url\n"; raise Not_found

  and default_headers_handler old_url level sock ans_code headers =
    let print_headers () =
      List.iter
        (fun (name, value) ->
          lprintf "[%s]=[%s]\n" name value;
        ) headers;
    in
    (* print_headers (); *)
    match ans_code with
      200 ->
        (*
        lprintf "ans_code: %d\n" ans_code;
        print_headers ();
        *)
        TcpBufferedSocket.set_closer sock
            (fun _ _ -> 
              (* lprintf "default_headers_handler closer\n"; *)
              f ()
            );

        let content_length = ref (-1) in
        List.iter (fun (name, content) ->
            if String.lowercase name = "content-length" then
              try
                content_length := int_of_string content
              with _ -> 
                  lprintf "bad content length [%s]\n" content;
        ) headers;
        let location = "Location", Url.to_string old_url in
        let content_handler = content_handler !content_length (location::headers) in
        set_reader sock content_handler;
        let buf = TcpBufferedSocket.buf sock in
        if buf.len > 0 then
          content_handler sock buf.len

    | 301 | 302 | 304 ->
        if !verbose then lprintf "Http_client %d: Redirect\n" ans_code;
        if level < 10 then
          begin
            try
              let url = ref "" in
              List.iter
                (fun (name, content) ->
                  if String.lowercase name = "location" then
                    url := content;
                ) headers;
              if !verbose then
                print_headers ();
              let url =
                if String.length !url > 0 && !url.[0] <> '/' then
                  !url
                else
                  Printf.sprintf "http://%s:%d%s"
                    old_url.Url.server old_url.Url.port !url
              in
              if !verbose then lprintf "Redirected to %s\n" url;
              let r = { r with req_url = Url.of_string url } in
              get_url (level+1) r
            
            with e ->
                lprintf "Http_client: error understanding redirect response %d\n" ans_code;
                print_headers ();
                raise Not_found
                
          end
        else 
          lprintf "Http_client: more than 10 redirections, aborting.";
          raise Not_found
          
    | 404 ->
        lprintf "Http_client: 404: Not found for: %s\n" (Url.to_string_no_args r.req_url);
        close sock (Closed_for_error "bad reply");
        raise Not_found

    | _ ->
        lprintf "Http_client: %d: bad reply for: %s\n"
          ans_code (Url.to_string_no_args r.req_url);
        close sock (Closed_for_error "bad reply");
        raise Not_found
  in 
  get_url 0 r;
  if !error = true then begin
      lprintf "Http_client: failed!!\n"; 
      raise Not_found
    end
  
  
let wget r f = 
  
  let file_buf = Buffer.create 1000 in
  let file_size = ref 0 in
  
  try
  get_page r
    (fun maxlen headers sock nread ->
(*      lprintf "received %d\n" nread; *)
      let buf = TcpBufferedSocket.buf sock in
      
      if nread > 0 then begin
          let left = 
            if maxlen >= 0 then
              mini (maxlen - !file_size) nread
            else nread
          in
          Buffer.add_string file_buf (String.sub buf.buf buf.pos left);
          buf_used buf left;
          file_size := !file_size + left;
          if nread > left then
            TcpBufferedSocket.close sock Closed_by_user
        end)
  (fun _ ->  
      let s = Buffer.contents file_buf in
      if s = "" then begin  
          lprintf "Empty content for url %s\n" 
            (Url.to_string r.req_url);
        end;
      let filename = Filename.temp_file "http_" ".tmp" in
      let oc = open_out_bin filename in
      output_string oc s;
      close_out oc;
      try
        (f filename : unit);
        Sys.remove filename
      with  e ->  lprintf
            "Exception %s in loading downloaded file %s"
            (Printexc2.to_string e) filename;
          Sys.remove filename;
          raise Not_found
  )
  with e -> lprintf "Http_client: error in wget\n"; raise Not_found

let whead r f = 
  
  get_page r
    (fun maxlen headers ->
      lprintf "Http_client.headers...\n";
      (try f headers with _ -> ());
      fun sock nread -> 
        close sock Closed_by_user
    )
  (fun _ ->  ())

let wget_string r f progress =
    
  let file_buf = Buffer.create 1000 in
  let file_size = ref 0 in

  get_page r
    (fun maxlen headers sock nread ->
        let buf = TcpBufferedSocket.buf sock in
        
        if nread > 0 then begin
            let left = 
              if maxlen >= 0 then
                mini (maxlen - !file_size) nread
              else nread
          in
          Buffer.add_string file_buf (String.sub buf.buf buf.pos left);
          progress left maxlen;
          buf_used buf left;
          file_size := !file_size + left;
          if nread > left then
            TcpBufferedSocket.close sock Closed_by_user
        end)
  (fun _ ->  
      f (Buffer.contents file_buf)
  )


let split_header header =
  for i = 0 to String.length header - 1 do
    if header.[i] = '\r' then header.[i] <- '\n';
  done;
  for i = String.length header - 1 downto 1 do
    if header.[i-1] = '\n' then 
      if header.[i] = ' ' then (header.[i] <- ','; header.[i-1] <- ',')
      else
      if header.[i] = ',' then header.[i-1] <- ',';
  done;
  String2.split_simplify header '\n'

let cut_headers headers =
  try
    List.map (fun s ->
        let pos = String.index s ':' in
        let len = String.length s in
        let key = String.sub s 0 pos in
        String.lowercase key, if pos+1 < len && s.[pos+1] = ' ' then
          String.sub s (pos+2) (len-pos-2), key
        else
          String.sub s (pos+1) (len-pos-1), key
    ) headers
  with e ->
      lprintf "Exception in cut_headers: %s\n" (Printexc2.to_string e);
      raise e
