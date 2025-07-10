(* Copyright 2025 Luca Carlon *)
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

open Filename2

let filesystem_compliant name fstype namemax =
  (* replace all illegal characters with a valid one.
     assumes all filesystems accept '_'s in filenames *)
  let escape_chars p filename =
    let s = Bytes.of_string filename in
    for i = 0 to String.length filename - 1 do
      if p (Bytes.get s i) then Bytes.set s i '_'
    done;
    Bytes.unsafe_to_string s
  in

  (* remove all illegal characters at the beginning of filename *)
  let trim_left p filename =
    let len = String.length filename in
    let left =
      let rec aux i =
        if i < len && p filename.[i] then aux (i+1) else i in
      aux 0 in
    if left = 0 then filename
    else
      String.sub filename left (len - left) in

  (* remove all illegal characters at the end of filename *)
  let trim_right p filename =
    let len = String.length filename in
    let right =
      let rec aux i =
        if i > 0 && p filename.[i-1] then aux (i-1) else i in
      aux len in
    if right = len then filename
    else
      String.sub filename 0 right in

  let minimal_filter c =
    match c with
      | '/' | '\\' | '<' | '>' | '"' -> true
      | _ -> false in

  let posix_compliant name =
    escape_chars minimal_filter name in

  let windows_compliant name =
    (* http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/creating__deleting__and_maintaining_files.asp *)
    let windows_filter c = 
      minimal_filter c ||
        match c with
          | '*' | '?' | '|' | ':' | '"' -> true
          | _ -> false in

    (* Windows has additional restrictions:
       - filenames cannot start with a '.' 
       - filenames cannot end with '.' or space *)
    let name = trim_left (fun c -> c = '.') name in
    let name = trim_right (fun c -> c = '.' || c = ' ') name in
    escape_chars windows_filter name in

  let macosx_compliant name =
  (* ':' is directory seperator on Mac OS X: http://www.comentum.com/File-Systems-HFS-FAT-UFS.html *)
    let macosx_filter c = 
      minimal_filter c || c = ':' in
    escape_chars macosx_filter name in

  let sys_checked_name =
    match fstype with
    | `Win -> windows_compliant name
    | `Mac -> macosx_compliant name
    | `Posix
    | `Unknown -> posix_compliant name
  in

  let fs_checked_name =
    let remove_last_spaces s =
      let len = String.length s in
      let rec aux n =
        if n = 0 then n
        else
          let n1 = n - 1 in
          if s.[n1] = ' ' then aux n1
          else n in
      let last_space = aux len in
      if last_space = len then s
      else String.sub s 0 last_space
    in
(* FAT filesystems do not allow files with space as last char *)
    match fstype with
    | `Win -> remove_last_spaces sys_checked_name
    | _ -> sys_checked_name
  in

  let length_checked_name =
    if namemax < 1 || String.length sys_checked_name < namemax then
      fs_checked_name
    else
      let ext = extension fs_checked_name in
        if String.length ext > namemax then
          String.sub fs_checked_name 0 namemax
        else
          String.sub fs_checked_name 0 (namemax - (String.length ext)) ^ ext
  in
  length_checked_name
