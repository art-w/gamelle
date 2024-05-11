open Common

let lock = Mutex.create ()
let current_run = ref No_run
let has_crashed = ref false
let get_current_run () = mutex_protect lock (fun () -> !current_run)
let crashed () = !has_crashed

let clean () =
  mutex_protect lock @@ fun () ->
  match !current_run with
  | No_run -> assert false
  | Run { clean; _ } ->
      List.iter (fun fn -> fn ()) clean;
      current_run := No_run

let is_catchable = function Exit | Out_of_memory -> false | _ -> true

let unsafe_update ~io =
  if not !has_crashed then
    match !current_run with
    | No_run -> invalid_arg "No game currently running"
    | Run ({ state; update; clean; _ } as run_state) ->
        let state =
          let io =
            Gamelle_common.View.translate
              (Gamelle_common.Geometry.Vec.v 0.0
                 !Gamelle_common.ui_replay_height)
              io
          in
          try update ~io state
          with exc when is_catchable exc ->
            Format.fprintf Format.err_formatter
              "@.@.UNHANDLED EXCEPTION:@.@.  %s@.@." (Printexc.to_string exc);
            Printexc.print_backtrace stderr;
            Format.fprintf Format.err_formatter "@.@.";
            Stdlib.flush stderr;
            has_crashed := true;
            state
        in
        let clean = List.rev_append !(io.clean) clean in
        current_run := Run { run_state with state; update; clean }

let update_frame ~io = mutex_protect lock (fun () -> unsafe_update ~io)

let force_reload () =
  mutex_protect lock @@ fun () ->
  match !current_run with
  | No_run -> ()
  | Run ({ random_seed; init_state; _ } as run_state) ->
      Random.set_state random_seed;
      current_run := Run { run_state with state = init_state }

let run state update ~start ~reload =
  Mutex.lock lock;
  has_crashed := false;
  let prev = !current_run in
  current_run :=
    Run
      {
        random_seed = Random.get_state ();
        init_state = state;
        state;
        update;
        clean = [];
      };
  match prev with
  | No_run ->
      Mutex.unlock lock;
      start ()
  | Run { clean; update; _ } ->
      List.iter (fun fn -> fn ()) clean;
      reload (Obj.magic update);
      Mutex.unlock lock
