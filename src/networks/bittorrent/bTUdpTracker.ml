(** UDP trackers
  http://www.bittorrent.org/beps/bep_0015.html *)
open Bitstring
  
let of_bits = string_of_bitstring
  
let bits = bitstring_of_string
  
exception Error of string
  
let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt
  
(** connect - obtain connection_id *)
let connect_request txn =
  of_bits
    (let __pabitstring_exn_1002 =
       Bitstring.Construct_failure ("value out of range",
         "src/networks/bittorrent/bTUdpTracker.mlp", 17, 12) in
     let __pabitstring_buffer_1001 = Bitstring.Buffer.create ()
     in
       (Bitstring.construct_int64_be_unsigned __pabitstring_buffer_1001
          0x41727101980L 64 __pabitstring_exn_1002;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1001 0l 32
          __pabitstring_exn_1002;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1001 txn
          32 __pabitstring_exn_1002;
        Bitstring.Buffer.contents __pabitstring_buffer_1001))
  
(** connect response with connection_id for future use *)
let connect_response s exp_txn =
  let (__pabitstring_data_1003, __pabitstring_original_off_1006,
       __pabitstring_original_len_1007) =
    bits s in
  let __pabitstring_off_1004 = __pabitstring_original_off_1006
  and __pabitstring_len_1005 = __pabitstring_original_len_1007 in
  let __pabitstring_off_aligned_1008 = (__pabitstring_off_1004 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1008;
     let __pabitstring_result_1009 = ref None
     in
       ((try
           (if __pabitstring_len_1005 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1008
                 then
                   (let o = (__pabitstring_original_off_1006 lsr 3) + 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1003 o)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1003 __pabitstring_off_1004
                     __pabitstring_len_1005 32 in
               let __pabitstring_off_1004 = __pabitstring_off_1004 + 32
               and __pabitstring_len_1005 = __pabitstring_len_1005 - 32
               in
                 match v with
                 | 0l when true ->
                     if __pabitstring_len_1005 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1008
                          then
                            (let o =
                               (__pabitstring_original_off_1006 lsr 3) + 4
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1003 o)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1003 __pabitstring_off_1004
                              __pabitstring_len_1005 32 in
                        let __pabitstring_off_1004 =
                          __pabitstring_off_1004 + 32
                        and __pabitstring_len_1005 =
                          __pabitstring_len_1005 - 32
                        in
                          match v with
                          | txn when true ->
                              if __pabitstring_len_1005 >= 64
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1008
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1006 lsr
                                           3)
                                          + 8
                                      in
                                        Bitstring.
                                          extract_fastpath_int64_be_unsigned
                                          __pabitstring_data_1003 o)
                                   else
                                     Bitstring.extract_int64_be_unsigned
                                       __pabitstring_data_1003
                                       __pabitstring_off_1004
                                       __pabitstring_len_1005 64 in
                                 let __pabitstring_off_1004 =
                                   __pabitstring_off_1004 + 64
                                 and __pabitstring_len_1005 =
                                   __pabitstring_len_1005 - 64
                                 in
                                   match v with
                                   | conn_id when true ->
                                       (__pabitstring_result_1009 :=
                                          Some
                                            (if txn = exp_txn
                                             then conn_id
                                             else
                                               fail
                                                 "error connect_response txn %ld expected %ld"
                                                 txn exp_txn);
                                        raise Exit)
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            if __pabitstring_len_1005 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1008
                 then
                   (let o = (__pabitstring_original_off_1006 lsr 3) + 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1003 o)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1003 __pabitstring_off_1004
                     __pabitstring_len_1005 32 in
               let __pabitstring_off_1004 = __pabitstring_off_1004 + 32
               and __pabitstring_len_1005 = __pabitstring_len_1005 - 32
               in
                 match v with
                 | 3l when true ->
                     if __pabitstring_len_1005 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1008
                          then
                            (let o =
                               (__pabitstring_original_off_1006 lsr 3) + 4
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1003 o)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1003 __pabitstring_off_1004
                              __pabitstring_len_1005 32 in
                        let __pabitstring_off_1004 =
                          __pabitstring_off_1004 + 32
                        and __pabitstring_len_1005 =
                          __pabitstring_len_1005 - 32
                        in
                          match v with
                          | txn when true ->
                              let __pabitstring_str_1010 =
                                Bitstring.string_of_bitstring
                                  (__pabitstring_data_1003,
                                   __pabitstring_off_1004,
                                   __pabitstring_len_1005) in
                              let __pabitstring_off_1004 =
                                __pabitstring_off_1004 +
                                  __pabitstring_len_1005 in
                              let __pabitstring_len_1005 = 0
                              in
                                (match __pabitstring_str_1010 with
                                 | msg when true ->
                                     (__pabitstring_result_1009 :=
                                        Some
                                          (fail
                                             "error connect_response txn %ld : %s"
                                             txn msg);
                                      raise Exit)
                                 | _ -> ())
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1009 :=
              Some
                (fail "error connect_response (expected txn %ld) : %s"
                   exp_txn (AnyEndian.dump_hex_s s));
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1009 with
        | Some x -> x
        | None ->
            raise
              (Match_failure ("src/networks/bittorrent/bTUdpTracker.mlp", 21,
                 2))))
  
(** announce *)
let announce_request conn txn ~info_hash ~peer_id
                     (downloaded, left, uploaded) event ?(ip = 0l)
                     ?(key = 0l) ~numwant port =
  of_bits
    (let __pabitstring_exn_1012 =
       Bitstring.Construct_failure ("value out of range",
         "src/networks/bittorrent/bTUdpTracker.mlp", 29, 11) in
     let __pabitstring_buffer_1011 = Bitstring.Buffer.create ()
     in
       (Bitstring.construct_int64_be_unsigned __pabitstring_buffer_1011 conn
          64 __pabitstring_exn_1012;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011 1l 32
          __pabitstring_exn_1012;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011 txn
          32 __pabitstring_exn_1012;
        (let __pabitstring_bs_1013 = info_hash
         in
           if (String.length __pabitstring_bs_1013) = 20
           then
             Bitstring.construct_string __pabitstring_buffer_1011
               __pabitstring_bs_1013
           else
             raise
               (Bitstring.Construct_failure
                  ("length of string does not match declaration",
                  "src/networks/bittorrent/bTUdpTracker.mlp", 33, 4)));
        (let __pabitstring_bs_1014 = peer_id
         in
           if (String.length __pabitstring_bs_1014) = 20
           then
             Bitstring.construct_string __pabitstring_buffer_1011
               __pabitstring_bs_1014
           else
             raise
               (Bitstring.Construct_failure
                  ("length of string does not match declaration",
                  "src/networks/bittorrent/bTUdpTracker.mlp", 34, 4)));
        Bitstring.construct_int64_be_unsigned __pabitstring_buffer_1011
          downloaded 64 __pabitstring_exn_1012;
        Bitstring.construct_int64_be_unsigned __pabitstring_buffer_1011 left
          64 __pabitstring_exn_1012;
        Bitstring.construct_int64_be_unsigned __pabitstring_buffer_1011
          uploaded 64 __pabitstring_exn_1012;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011 event
          32 __pabitstring_exn_1012;
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011 0l 32
          __pabitstring_exn_1012;
        (* ip *)
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011 key
          32 __pabitstring_exn_1012;
        (* key *)
        Bitstring.construct_int32_be_unsigned __pabitstring_buffer_1011
          numwant 32 __pabitstring_exn_1012;
        (* numwant *)
        Bitstring.construct_int_be_unsigned __pabitstring_buffer_1011 port 16
          __pabitstring_exn_1012;
        Bitstring.Buffer.contents __pabitstring_buffer_1011))
  
(** announce response *)
let announce_response s exp_txn =
  let rec clients rest l =
    let (__pabitstring_data_1015, __pabitstring_original_off_1018,
         __pabitstring_original_len_1019) =
      rest in
    let __pabitstring_off_1016 = __pabitstring_original_off_1018
    and __pabitstring_len_1017 = __pabitstring_original_len_1019 in
    let __pabitstring_off_aligned_1020 = (__pabitstring_off_1016 land 7) = 0
    in
      (ignore __pabitstring_off_aligned_1020;
       let __pabitstring_result_1021 = ref None
       in
         ((try
             (if __pabitstring_len_1017 >= 32
              then
                (let v =
                   if __pabitstring_off_aligned_1020
                   then
                     (let o = (__pabitstring_original_off_1018 lsr 3) + 0
                      in
                        Bitstring.extract_fastpath_int32_be_unsigned
                          __pabitstring_data_1015 o)
                   else
                     Bitstring.extract_int32_be_unsigned
                       __pabitstring_data_1015 __pabitstring_off_1016
                       __pabitstring_len_1017 32 in
                 let __pabitstring_off_1016 = __pabitstring_off_1016 + 32
                 and __pabitstring_len_1017 = __pabitstring_len_1017 - 32
                 in
                   match v with
                   | ip when true ->
                       if __pabitstring_len_1017 >= 16
                       then
                         (let v =
                            if __pabitstring_off_aligned_1020
                            then
                              (let o =
                                 (__pabitstring_original_off_1018 lsr 3) + 4
                               in
                                 Bitstring.extract_fastpath_int16_be_unsigned
                                   __pabitstring_data_1015 o)
                            else
                              Bitstring.extract_int_be_unsigned
                                __pabitstring_data_1015
                                __pabitstring_off_1016 __pabitstring_len_1017
                                16 in
                          let __pabitstring_off_1016 =
                            __pabitstring_off_1016 + 16
                          and __pabitstring_len_1017 =
                            __pabitstring_len_1017 - 16
                          in
                            match v with
                            | port when true ->
                                let rest =
                                  (__pabitstring_data_1015,
                                   __pabitstring_off_1016,
                                   __pabitstring_len_1017) in
                                let __pabitstring_off_1016 =
                                  __pabitstring_off_1016 +
                                    __pabitstring_len_1017 in
                                let __pabitstring_len_1017 = 0
                                in
                                  (__pabitstring_result_1021 :=
                                     Some (clients rest ((ip, port) :: l));
                                   raise Exit)
                            | _ -> ())
                       else ()
                   | _ -> ())
              else ();
              __pabitstring_result_1021 := Some l;
              raise Exit)
           with | Exit -> ());
          match !__pabitstring_result_1021 with
          | Some x -> x
          | None ->
              raise
                (Match_failure ("src/networks/bittorrent/bTUdpTracker.mlp",
                   47, 4)))) in
  let (__pabitstring_data_1022, __pabitstring_original_off_1025,
       __pabitstring_original_len_1026) =
    bits s in
  let __pabitstring_off_1023 = __pabitstring_original_off_1025
  and __pabitstring_len_1024 = __pabitstring_original_len_1026 in
  let __pabitstring_off_aligned_1027 = (__pabitstring_off_1023 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1027;
     let __pabitstring_result_1028 = ref None
     in
       ((try
           (if __pabitstring_len_1024 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1027
                 then
                   (let o = (__pabitstring_original_off_1025 lsr 3) + 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1022 o)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1022 __pabitstring_off_1023
                     __pabitstring_len_1024 32 in
               let __pabitstring_off_1023 = __pabitstring_off_1023 + 32
               and __pabitstring_len_1024 = __pabitstring_len_1024 - 32
               in
                 match v with
                 | 1l when true ->
                     if __pabitstring_len_1024 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1027
                          then
                            (let o =
                               (__pabitstring_original_off_1025 lsr 3) + 4
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1022 o)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1022 __pabitstring_off_1023
                              __pabitstring_len_1024 32 in
                        let __pabitstring_off_1023 =
                          __pabitstring_off_1023 + 32
                        and __pabitstring_len_1024 =
                          __pabitstring_len_1024 - 32
                        in
                          match v with
                          | txn when true ->
                              if __pabitstring_len_1024 >= 32
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1027
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1025 lsr
                                           3)
                                          + 8
                                      in
                                        Bitstring.
                                          extract_fastpath_int32_be_unsigned
                                          __pabitstring_data_1022 o)
                                   else
                                     Bitstring.extract_int32_be_unsigned
                                       __pabitstring_data_1022
                                       __pabitstring_off_1023
                                       __pabitstring_len_1024 32 in
                                 let __pabitstring_off_1023 =
                                   __pabitstring_off_1023 + 32
                                 and __pabitstring_len_1024 =
                                   __pabitstring_len_1024 - 32
                                 in
                                   match v with
                                   | interval when true ->
                                       if __pabitstring_len_1024 >= 32
                                       then
                                         (let v =
                                            if __pabitstring_off_aligned_1027
                                            then
                                              (let o =
                                                 (__pabitstring_original_off_1025
                                                    lsr 3)
                                                   + 12
                                               in
                                                 Bitstring.
                                                   extract_fastpath_int32_be_unsigned
                                                   __pabitstring_data_1022 o)
                                            else
                                              Bitstring.
                                                extract_int32_be_unsigned
                                                __pabitstring_data_1022
                                                __pabitstring_off_1023
                                                __pabitstring_len_1024 32 in
                                          let __pabitstring_off_1023 
                                            = __pabitstring_off_1023 + 32
                                          and __pabitstring_len_1024 
                                            = __pabitstring_len_1024 - 32
                                          in
                                            match v with
                                            | leechers when true ->
                                                if
                                                  __pabitstring_len_1024 >=
                                                    32
                                                then
                                                  (let v =
                                                     if
                                                       __pabitstring_off_aligned_1027
                                                     then
                                                       (let o =
                                                          (__pabitstring_original_off_1025
                                                             lsr 3)
                                                            + 16
                                                        in
                                                          Bitstring.
                                                            extract_fastpath_int32_be_unsigned
                                                            __pabitstring_data_1022
                                                            o)
                                                     else
                                                       Bitstring.
                                                         extract_int32_be_unsigned
                                                         __pabitstring_data_1022
                                                         __pabitstring_off_1023
                                                         __pabitstring_len_1024
                                                         32 in
                                                   let __pabitstring_off_1023 
                                                     =
                                                     __pabitstring_off_1023 +
                                                       32
                                                   and
                                                     __pabitstring_len_1024 
                                                     =
                                                     __pabitstring_len_1024 -
                                                       32
                                                   in
                                                     match v with
                                                     | seeders when true ->
                                                         let rest =
                                                           (__pabitstring_data_1022,
                                                            __pabitstring_off_1023,
                                                            __pabitstring_len_1024) in
                                                         let __pabitstring_off_1023 
                                                           =
                                                           __pabitstring_off_1023
                                                             +
                                                             __pabitstring_len_1024 in
                                                         let __pabitstring_len_1024 
                                                           = 0
                                                         in
                                                           (__pabitstring_result_1028 :=
                                                              Some
                                                                (if
                                                                   txn =
                                                                    exp_txn
                                                                 then
                                                                   (interval,
                                                                    (
                                                                    clients
                                                                    rest []))
                                                                 else
                                                                   fail
                                                                    "error announce_response txn %ld expected %ld"
                                                                    txn
                                                                    exp_txn);
                                                            raise Exit)
                                                     | _ -> ())
                                                else ()
                                            | _ -> ())
                                       else ()
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            if __pabitstring_len_1024 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1027
                 then
                   (let o = (__pabitstring_original_off_1025 lsr 3) + 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1022 o)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1022 __pabitstring_off_1023
                     __pabitstring_len_1024 32 in
               let __pabitstring_off_1023 = __pabitstring_off_1023 + 32
               and __pabitstring_len_1024 = __pabitstring_len_1024 - 32
               in
                 match v with
                 | 3l when true ->
                     if __pabitstring_len_1024 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1027
                          then
                            (let o =
                               (__pabitstring_original_off_1025 lsr 3) + 4
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1022 o)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1022 __pabitstring_off_1023
                              __pabitstring_len_1024 32 in
                        let __pabitstring_off_1023 =
                          __pabitstring_off_1023 + 32
                        and __pabitstring_len_1024 =
                          __pabitstring_len_1024 - 32
                        in
                          match v with
                          | txn when true ->
                              let __pabitstring_str_1029 =
                                Bitstring.string_of_bitstring
                                  (__pabitstring_data_1022,
                                   __pabitstring_off_1023,
                                   __pabitstring_len_1024) in
                              let __pabitstring_off_1023 =
                                __pabitstring_off_1023 +
                                  __pabitstring_len_1024 in
                              let __pabitstring_len_1024 = 0
                              in
                                (match __pabitstring_str_1029 with
                                 | msg when true ->
                                     (__pabitstring_result_1028 :=
                                        Some
                                          (fail
                                             "error announce_response txn %ld : %s"
                                             txn msg);
                                      raise Exit)
                                 | _ -> ())
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1028 :=
              Some
                (fail "error announce_response (expected txn %ld) : %s"
                   exp_txn (AnyEndian.dump_hex_s s));
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1028 with
        | Some x -> x
        | None ->
            raise
              (Match_failure ("src/networks/bittorrent/bTUdpTracker.mlp", 51,
                 2))))
  

