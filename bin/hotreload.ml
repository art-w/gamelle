let watch cmxs_file =
  let th =
    Thread.create @@ fun () ->
    let target_file = Filename.concat (Sys.getcwd ()) cmxs_file in
    let count = ref 0 in
    while true do
      try
        incr count;
        let new_file =
          Filename.concat
            (Filename.dirname target_file)
            ("g" ^ string_of_int !count ^ "_" ^ Filename.basename target_file)
        in
        let copy_ok =
          Sys.command (Printf.sprintf "cp %s %s" target_file new_file)
        in
        if copy_ok = 0 then Dynlink.loadfile_private new_file;
        let fd_watcher = Inotify.create () in
        let watch = Inotify.add_watch fd_watcher target_file [ S_All ] in
        let _evs = Inotify.read fd_watcher in
        (* Unix.sleepf 0.1; *)
        Inotify.rm_watch fd_watcher watch;
        Unix.close fd_watcher
      with err ->
        Format.printf "ERROR: %s@." (Printexc.to_string err);
        Unix.sleepf 0.01
    done
  in
  let _th = th () in
  ()

let run cmxs_file =
  watch cmxs_file;
  Gamelle.run () ~update:(fun _ () -> ()) ~render:(fun () -> ())
