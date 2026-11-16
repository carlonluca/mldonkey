(* main.ml *)

open Printf2

let () =
  BasicSocket.force_link ();
  BasicSocket.add_timer 0.1 (fun _ ->
    Printf2.force_link ();
    Bitv.force_link ();
    Magiclib.force_link ();
    Charset.force_link ();
    Md4.force_link ();
    TcpServerSocket.force_link ();
    Ip_set.force_link ();
    UdpSocket.force_link ();
    Geoip.force_link ();
    Ip.force_link ();
    TcpBufferedSocket.force_link ();
    Cobs.force_link ();
    BasicSocket.force_link ();
    Rss_io.force_link ();
    Rss_date.force_link ();
    Zlib.force_link ();
    Bzlib.force_link ();
    Gd.force_link ();
    Filename2.force_link ();
    Heap.force_link ();
    Unix32.force_link ();
    (* Mp3.force_link (); *)
    (* Mp3_args.force_link (); *)
    DriverCommands.force_link ();
    DriverInterface.force_link ();
    CommonUser.force_link ();
    CommonResult.force_link ();
    CommonHosts.force_link ();
    CommonSources.force_link ();
    CommonShared.force_link ();
    CommonFile.force_link ();
    GuiEncoding.force_link ();
    CommonNetwork.force_link ();
    CommonClient.force_link ();
    CommonComplexOptions.force_link ();
    CommonHasher.force_link ();
    CommonInteractive.force_link ();
    CommonUploads.force_link ();
    CommonGlobals.force_link ();
    CommonIndexing.force_link ();
    CommonSwarming.force_link ();
    CommonBlocking.force_link ();
    CommonServer.force_link ();
    GuiDecoding.force_link ();
    CommonWeb.force_link ();
    CommonSearch.force_link ();
    CommonOptions.force_link ();
    CommonRoom.force_link ();
    CommonStats.force_link ();
    CommonPictures.force_link ();
    CommonUserDb.force_link ();
    DriverMain.init_1 ();
    DriverMain.init_2 ();
    );
  CommonMain.main_loop ()
