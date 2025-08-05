open Printf2
  
open CommonOptions
  
open Bitstring
  
let lprintf_nl fmt = lprintf_nl2 "[NODES]" fmt
  
(* see http://wiki.amule.org/index.php/Nodes.dat_file *)
let parse filename f =
  let (__pabitstring_data_1015, __pabitstring_original_off_1018,
       __pabitstring_original_len_1019) =
    bitstring_of_file filename in
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
                 | 0l when true ->
                     if __pabitstring_len_1017 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1020
                          then
                            (let o =
                               (__pabitstring_original_off_1018 lsr 3) + 4
                             in
                               Bitstring.extract_fastpath_int32_le_unsigned
                                 __pabitstring_data_1015 o)
                          else
                            Bitstring.extract_int32_le_unsigned
                              __pabitstring_data_1015 __pabitstring_off_1016
                              __pabitstring_len_1017 32 in
                        let __pabitstring_off_1016 =
                          __pabitstring_off_1016 + 32
                        and __pabitstring_len_1017 =
                          __pabitstring_len_1017 - 32
                        in
                          match v with
                          | 02l when true ->
                              if __pabitstring_len_1017 >= 32
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1020
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1018 lsr
                                           3)
                                          + 8
                                      in
                                        Bitstring.
                                          extract_fastpath_int32_le_unsigned
                                          __pabitstring_data_1015 o)
                                   else
                                     Bitstring.extract_int32_le_unsigned
                                       __pabitstring_data_1015
                                       __pabitstring_off_1016
                                       __pabitstring_len_1017 32 in
                                 let __pabitstring_off_1016 =
                                   __pabitstring_off_1016 + 32
                                 and __pabitstring_len_1017 =
                                   __pabitstring_len_1017 - 32
                                 in
                                   match v with
                                   | count when true ->
                                       let count = Int32.to_int count
                                       in
                                         if
                                           (((34 * 8) * count) >= 0) &&
                                             (((34 * 8) * count) <=
                                                __pabitstring_len_1017)
                                         then
                                           (let nodes =
                                              (__pabitstring_data_1015,
                                               __pabitstring_off_1016,
                                               ((34 * 8) * count)) in
                                            let __pabitstring_off_1016 
                                              =
                                              __pabitstring_off_1016 +
                                                ((34 * 8) * count)
                                            and __pabitstring_len_1017 
                                              =
                                              __pabitstring_len_1017 -
                                                ((34 * 8) * count) in
                                            let rest =
                                              (__pabitstring_data_1015,
                                               __pabitstring_off_1016,
                                               __pabitstring_len_1017) in
                                            let __pabitstring_off_1016 
                                              =
                                              __pabitstring_off_1016 +
                                                __pabitstring_len_1017 in
                                            let __pabitstring_len_1017 = 0
                                            in
                                              if (bitstring_length rest) = 0
                                              then
                                                (__pabitstring_result_1021 :=
                                                   Some
                                                     (let () =
                                                        for i = 0 to
                                                          pred count do
                                                          let (__pabitstring_data_1001,
                                                               __pabitstring_original_off_1004,
                                                               __pabitstring_original_len_1005) =
                                                            subbitstring
                                                              nodes
                                                              ((34 * 8) * i)
                                                              (34 * 8) in
                                                          let __pabitstring_off_1002 
                                                            =
                                                            __pabitstring_original_off_1004
                                                          and
                                                            __pabitstring_len_1003 
                                                            =
                                                            __pabitstring_original_len_1005 in
                                                          let __pabitstring_off_aligned_1006 
                                                            =
                                                            (__pabitstring_off_1002
                                                               land 7)
                                                              = 0
                                                          in
                                                            (ignore
                                                               __pabitstring_off_aligned_1006;
                                                             let __pabitstring_result_1007 
                                                               = ref None
                                                             in
                                                               ((try
                                                                   (if
                                                                    __pabitstring_len_1003
                                                                    >= 128
                                                                    then
                                                                    (let str 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    0
                                                                    in
                                                                    Bytes.
                                                                    sub_string
                                                                    __pabitstring_data_1001
                                                                    o 16)
                                                                    else
                                                                    Bitstring.
                                                                    string_of_bitstring
                                                                    (__pabitstring_data_1001,
                                                                    __pabitstring_off_1002,
                                                                    128) in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 128
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 128
                                                                    in
                                                                    match str
                                                                    with
                                                                    | 
                                                                    id when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    16
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip1 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    17
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip2 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    18
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip3 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    19
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip4 when
                                                                    true ->
                                                                    (* littleendian *)
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    20
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_le_unsigned
                                                                    __pabitstring_data_1001
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_le_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    16 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    udp when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    22
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_le_unsigned
                                                                    __pabitstring_data_1001
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_le_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    16 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    tcp when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    24
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    version
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 64
                                                                    then
                                                                    (let str 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    25
                                                                    in
                                                                    Bytes.
                                                                    sub_string
                                                                    __pabitstring_data_1001
                                                                    o 8)
                                                                    else
                                                                    Bitstring.
                                                                    string_of_bitstring
                                                                    (__pabitstring_data_1001,
                                                                    __pabitstring_off_1002,
                                                                    64) in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 64
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 64
                                                                    in
                                                                    match str
                                                                    with
                                                                    | 
                                                                    key when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    33
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    verified
                                                                    when true
                                                                    ->
                                                                    (__pabitstring_result_1007 :=
                                                                    Some
                                                                    (let ip 
                                                                    =
                                                                    Ip.
                                                                    of_ints
                                                                    (ip4,
                                                                    ip3, ip2,
                                                                    ip1)
                                                                    in
                                                                    (if
                                                                    !
                                                                    verbose_overnet
                                                                    then
                                                                    lprintf_nl
                                                                    "v2: id %S ip %s udp %d tcp %d ver %d key %S chk %d"
                                                                    id
                                                                    (Ip.
                                                                    to_string
                                                                    ip) udp
                                                                    tcp
                                                                    version
                                                                    key
                                                                    verified
                                                                    else ();
                                                                    f ip udp));
                                                                    raise
                                                                    Exit)
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ();
                                                                    __pabitstring_result_1007 :=
                                                                    Some
                                                                    (failwith
                                                                    "v2: bad contact");
                                                                    raise
                                                                    Exit)
                                                                 with
                                                                 | Exit -> ());
                                                                match 
                                                                  !
                                                                    __pabitstring_result_1007
                                                                with
                                                                | Some x -> x
                                                                | None ->
                                                                    raise
                                                                    (Match_failure
                                                                    ("src/networks/donkey/donkeyNodesDat.mlp",
                                                                    17, 9))))
                                                        done
                                                      in ());
                                                 raise Exit)
                                              else ())
                                         else ()
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            if __pabitstring_len_1017 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1020
                 then
                   (let o = (__pabitstring_original_off_1018 lsr 3) + 0
                    in
                      Bitstring.extract_fastpath_int32_le_unsigned
                        __pabitstring_data_1015 o)
                 else
                   Bitstring.extract_int32_le_unsigned
                     __pabitstring_data_1015 __pabitstring_off_1016
                     __pabitstring_len_1017 32 in
               let __pabitstring_off_1016 = __pabitstring_off_1016 + 32
               and __pabitstring_len_1017 = __pabitstring_len_1017 - 32
               in
                 match v with
                 | count when true ->
                     let count = Int32.to_int count
                     in
                       if
                         (((25 * 8) * count) >= 0) &&
                           (((25 * 8) * count) <= __pabitstring_len_1017)
                       then
                         (let nodes =
                            (__pabitstring_data_1015, __pabitstring_off_1016,
                             ((25 * 8) * count)) in
                          let __pabitstring_off_1016 =
                            __pabitstring_off_1016 + ((25 * 8) * count)
                          and __pabitstring_len_1017 =
                            __pabitstring_len_1017 - ((25 * 8) * count) in
                          let rest =
                            (__pabitstring_data_1015, __pabitstring_off_1016,
                             __pabitstring_len_1017) in
                          let __pabitstring_off_1016 =
                            __pabitstring_off_1016 + __pabitstring_len_1017 in
                          let __pabitstring_len_1017 = 0
                          in
                            if (bitstring_length rest) = 0
                            then
                              (__pabitstring_result_1021 :=
                                 Some
                                   (let () =
                                      for i = 0 to pred count do
                                        let (__pabitstring_data_1008,
                                             __pabitstring_original_off_1011,
                                             __pabitstring_original_len_1012) =
                                          subbitstring nodes ((25 * 8) * i)
                                            (25 * 8) in
                                        let __pabitstring_off_1009 =
                                          __pabitstring_original_off_1011
                                        and __pabitstring_len_1010 =
                                          __pabitstring_original_len_1012 in
                                        let __pabitstring_off_aligned_1013 
                                          =
                                          (__pabitstring_off_1009 land 7) = 0
                                        in
                                          (ignore
                                             __pabitstring_off_aligned_1013;
                                           let __pabitstring_result_1014 
                                             = ref None
                                           in
                                             ((try
                                                 (if
                                                    __pabitstring_len_1010 >=
                                                      128
                                                  then
                                                    (let str =
                                                       if
                                                         __pabitstring_off_aligned_1013
                                                       then
                                                         (let o =
                                                            (__pabitstring_original_off_1011
                                                               lsr 3)
                                                              + 0
                                                          in
                                                            Bytes.sub_string
                                                              __pabitstring_data_1008
                                                              o 16)
                                                       else
                                                         Bitstring.
                                                           string_of_bitstring
                                                           (__pabitstring_data_1008,
                                                            __pabitstring_off_1009,
                                                            128) in
                                                     let __pabitstring_off_1009 
                                                       =
                                                       __pabitstring_off_1009
                                                         + 128
                                                     and
                                                       __pabitstring_len_1010 
                                                       =
                                                       __pabitstring_len_1010
                                                         - 128
                                                     in
                                                       match str with
                                                       | id when true ->
                                                           if
                                                             __pabitstring_len_1010
                                                               >= 8
                                                           then
                                                             (let v =
                                                                if
                                                                  (not false)
                                                                    &&
                                                                    __pabitstring_off_aligned_1013
                                                                then
                                                                  (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    16
                                                                   in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1008
                                                                    o))
                                                                else
                                                                  Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    8 in
                                                              let __pabitstring_off_1009 
                                                                =
                                                                __pabitstring_off_1009
                                                                  + 8
                                                              and
                                                                __pabitstring_len_1010 
                                                                =
                                                                __pabitstring_len_1010
                                                                  - 8
                                                              in
                                                                match v with
                                                                | ip1 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    17
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1008
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    8 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip2 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    18
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1008
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    8 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip3 when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    19
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1008
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    8 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    ip4 when
                                                                    true ->
                                                                    (* littleendian *)
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    20
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_le_unsigned
                                                                    __pabitstring_data_1008
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_le_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    16 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    udp when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    22
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_le_unsigned
                                                                    __pabitstring_data_1008
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_le_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    16 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    tcp when
                                                                    true ->
                                                                    if
                                                                    __pabitstring_len_1010
                                                                    >= 8
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    (not
                                                                    false) &&
                                                                    __pabitstring_off_aligned_1013
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1011
                                                                    lsr 3) +
                                                                    24
                                                                    in
                                                                    Bitstring.
                                                                    char_code
                                                                    (Bytes.
                                                                    unsafe_get
                                                                    __pabitstring_data_1008
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1008
                                                                    __pabitstring_off_1009
                                                                    __pabitstring_len_1010
                                                                    8 in
                                                                    let __pabitstring_off_1009 
                                                                    =
                                                                    __pabitstring_off_1009
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1010 
                                                                    =
                                                                    __pabitstring_len_1010
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    typ when
                                                                    true ->
                                                                    (__pabitstring_result_1014 :=
                                                                    Some
                                                                    (let ip 
                                                                    =
                                                                    Ip.
                                                                    of_ints
                                                                    (ip4,
                                                                    ip3, ip2,
                                                                    ip1)
                                                                    in
                                                                    (if
                                                                    !
                                                                    verbose_overnet
                                                                    then
                                                                    lprintf_nl
                                                                    "v0: id %S ip %s udp %d tcp %d typ %d"
                                                                    id
                                                                    (Ip.
                                                                    to_string
                                                                    ip) udp
                                                                    tcp typ
                                                                    else ();
                                                                    f ip udp));
                                                                    raise
                                                                    Exit)
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                | _ -> ())
                                                           else ()
                                                       | _ -> ())
                                                  else ();
                                                  __pabitstring_result_1014 :=
                                                    Some
                                                      (failwith
                                                         "v0: bad contact");
                                                  raise Exit)
                                               with | Exit -> ());
                                              match !
                                                      __pabitstring_result_1014
                                              with
                                              | Some x -> x
                                              | None ->
                                                  raise
                                                    (Match_failure
                                                       ("src/networks/donkey/donkeyNodesDat.mlp",
                                                       40, 9))))
                                      done
                                    in ());
                               raise Exit)
                            else ())
                       else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1021 :=
              Some (failwith "nodes.dat bad header");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1021 with
        | Some x -> x
        | None ->
            raise
              (Match_failure ("src/networks/donkey/donkeyNodesDat.mlp", 11,
                 2))))
  


let force_link () = ()

