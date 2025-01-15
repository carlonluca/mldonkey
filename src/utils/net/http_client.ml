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

(* HTTP Requests: GET, HEAD *)

open Printf2
open BasicSocket
open Url
open TcpBufferedSocket
open Int64ops

type http_request =
  GET
| HEAD

type error = [
  `HTTP of int
| `RST of BasicSocket.close_reason
| `DNS
| `Block of Ip.t
| `CurlCode of Curl.curlCode
| `UnknownError
]

let show_error = function
| `HTTP code -> Printf.sprintf "HTTP error code %d" code
| `RST reason -> Printf.sprintf "Connection closed : %s" (BasicSocket.string_of_reason reason)
| `DNS -> Printf.sprintf "DNS resolution failed"
| `Block ip -> Printf.sprintf "Blocked connection to %s" (Ip.to_string ip)
| `CurlCode curlCode -> Printf.sprintf "Curl error: %s" (Curl.strerror curlCode)
| `UnknownError -> Printf.sprintf "Unknown error occurred"

let verbose = ref false

type request = {
    req_headers : ( string * string ) list;
    req_user_agent : string;
    req_accept : string;
    req_proxy : (string * int * (string * string) option) option; (* (host,port,(login,password)) *)
    mutable req_url : url;
    mutable req_gzip : bool;
    mutable req_save_to_file_time : float;
    req_request : http_request;
    req_referer : Url.url option;
    req_retry : int;
    req_max_retry : int;
    req_save : bool;
    req_max_total_time : float;
    req_filter_ip : (Ip.t -> bool);
  }

type content_handler = 
  int64 -> (string * string) list -> TcpBufferedSocket.t -> int -> unit

let log_prefix = "[HTTPcl]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let basic_request = {
    req_url = Url.of_string "http://mldonkey.sf.net/";
    req_referer = None;
    req_save_to_file_time = 0.;
    req_request = GET;
    req_gzip = false;
    req_proxy = None;
    req_headers = [];
    req_user_agent = "Wget 1.4";
    req_accept = "*/*";
    req_retry = 0;
    req_max_retry = 0;
    req_save = false;
    req_max_total_time = infinite_timeout;
    req_filter_ip = (fun _ -> true);
  }

let file_size filename =
  try
    let stats = Unix.stat filename in
    stats.Unix.st_size
  with
  | Unix.Unix_error (err, _, _) ->
    lprintf_nl "Error checking file size: %s" (Unix.error_message err);
    -1

let is_file_empty filename =
  let filesize = file_size filename in
  filesize <= 0

let def_ferr = (fun _ -> ())

(** Internal HTTP call implementation *)
let rec http_call_internal r write_f fretry retries_left progress =
  fretry ();
  let curl = Curl.init () in
  try
    (* TODO: TEST *)
    Curl.set_url curl (Url.to_string r.req_url);
    let headers = 
      ("User-Agent", r.req_user_agent) ::
      ("Accept", r.req_accept) ::
      r.req_headers
    in
    Curl.set_followlocation curl true;
    Curl.set_progressfunction curl (fun dltotal dlnow _ _ ->
      progress (int_of_float dlnow) (Int64.of_float dltotal);
      false
    );
    Curl.set_httpheader curl (List.map (fun (k, v) -> k ^ ": " ^ v) headers);
    if r.req_gzip then
      Curl.set_encoding curl Curl.CURL_ENCODING_GZIP;

    (* TODO: TEST *)
    (match r.req_request with
    | GET -> ()
    | HEAD ->
      Curl.set_nobody curl true;
      Curl.set_header curl true;
    );

    (* TODO: TEST *)
    (match r.req_referer with
    | Some ref_url -> Curl.set_referer curl (Url.to_string_no_args ref_url)
    | None -> ());

    (* TODO: TEST *)
    (match r.req_proxy with
    | Some (host, port, Some (username, password)) ->
        Curl.set_proxy curl host;
        Curl.set_proxyport curl port;
        Curl.set_proxyuserpwd curl (username ^ ":" ^ password)
    | Some (host, port, None) ->
        Curl.set_proxy curl host;
        Curl.set_proxyport curl port
    | None -> ());

    (* TODO: TEST *)
    if (int_of_float r.req_max_total_time) <> (int_of_float infinite_timeout) then
      Curl.set_timeout curl (int_of_float r.req_max_total_time);

    Curl.set_writefunction curl write_f;
    Curl.perform curl;
    match Curl.getinfo curl Curl.CURLINFO_HTTP_CODE with
    | Curl.CURLINFO_Long code -> match code with
      | 200 ->
        Curl.cleanup curl;
        Ok ""
      | code ->
        lprintf_nl "HTTP error occurred: %d" code;
        Curl.cleanup curl;
        Error (`HTTP code)
  with
  | Curl.CurlException (code, i, s) ->
    lprintf_nl "Request failed with code %s - %d - %s, retrying (%d left)..."
      (Curl.strerror code) i s (retries_left - 1);
    if retries_left > 0 then
      http_call_internal r write_f fretry (retries_left - 1) progress
    else begin
      Curl.cleanup curl;
      Error (`CurlCode code)
    end
  | ex ->
    lprintf_nl "Failed to download file: %s" (Printexc.to_string ex);
    Curl.cleanup curl;
    Error (`CurlCode Curl.CURLE_FAILED_INIT)

(** Call an endpoint *)
let http_call r write_f fok fko fretry progress =
  match http_call_internal r write_f fretry r.req_retry progress with
  | Ok _ -> fok ()
  | Error code -> fko code

(** Download to file *)
let wget r f =
  lprintf_nl "lcarlon: wget %s" (Url.to_string r.req_url);
  let webinfos_dir = "web_infos" in
  Unix2.safe_mkdir webinfos_dir;
  Unix2.can_write_to_directory webinfos_dir;
  let base = Filename.basename r.req_url.Url.short_file in
  (* Base could be "." for http://site.com/ *)
  let base = if base = "." 
    then begin
      let prng = Random.State.make_self_init () in
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      Printf.sprintf "http_%06x.tmp" rnd 
    end else base 
  in
  let tmp_file = Filename.concat webinfos_dir base in
  let oc = open_out_bin tmp_file in
  let write_f = (fun data ->
    lprintf_nl "lcarlon: downloaded %d" (String.length data);
    output_string oc data;
    String.length data
  ) in
  let fok () =
    try
      close_out oc;
      let size = file_size tmp_file in
      if size = 0 then
        lprintf_nl "Downloaded file %s is empty" tmp_file
      else
        lprintf_nl "Downloaded file %s size is %d" tmp_file size;
      if r.req_save_to_file_time <> 0. then
        Unix.utimes tmp_file r.req_save_to_file_time r.req_save_to_file_time;
      (f tmp_file : unit);
      if not r.req_save then Sys.remove tmp_file
    with e ->  
      lprintf_nl "Exception %s in loading downloaded file %s" (Printexc2.to_string e) tmp_file
  in
  let fko err =
    close_out oc;
    Sys.remove tmp_file;
  in
  lprintf_nl "wget";
  let fretry () = seek_out oc 0 in
  http_call r write_f fok fko fretry (fun _ _ -> ())

(** GET request to buffer *)
let wget_string r f ?(ferr=def_ferr) progress =
  let buffer = Buffer.create 1000 in
  let write_f = (fun data ->
    Buffer.add_string buffer data;
    String.length data
  ) in
  let fok () =
    f (Buffer.contents buffer)
  in
  let fko err =
    ferr err
  in
  lprintf_nl "wget_string";
  let fretry () = () in
  http_call r write_f fok fko fretry progress

(** HEAD request with error callback *)
let whead2 r f ferr =
  lprintf_nl "lcarlon: whead %s" (Url.to_string r.req_url);
  let f_headers = fun data ->
    let lines = String.split_on_char '\n' data in
    f (List.filter_map (fun line ->
      match String.index_opt line ':' with
      | Some idx ->
          let key = String.sub line 0 idx |> String.trim in
          let value = String.sub line (idx + 1) (String.length line - idx - 1) |> String.trim in
          Some (key, value)
      | None -> None;
    ) lines)
  in
  wget_string r f_headers ~ferr:ferr (fun _ _ -> ())

(** HEAD request *)
let whead r f = whead2 r f def_ferr

let split_header header =
  let len = String.length header in
  let header_bytes = Bytes.of_string header in
  for i = 0 to len - 1 do
    if Bytes.get header_bytes i = '\r' then
      Bytes.set header_bytes i '\n'
  done;
  for i = len - 1 downto 1 do
    if Bytes.get header_bytes (i - 1) = '\n' then
      if Bytes.get header_bytes i = ' ' then (
        Bytes.set header_bytes i ',';
        Bytes.set header_bytes (i - 1) ','
      ) else if Bytes.get header_bytes i = ',' then
        Bytes.set header_bytes (i - 1) ','
  done;
  String2.split_simplify (Bytes.unsafe_to_string header_bytes) '\n'

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
      lprintf_nl "Exception in cut_headers: %s" (Printexc2.to_string e);
      raise e
