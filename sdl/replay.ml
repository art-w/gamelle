open Gg
open Common

let clock = ref 0
let replay_events = ref []
let must_replay = ref []

let add e =
  incr clock;
  let e = { e with Gamelle_common.Events_backend.clock = 0 } in
  replay_events :=
    match !replay_events with
    | (e', count) :: es when e' = e -> (e', count + 1) :: es
    | es -> (e, 1) :: es

let reload () =
  clock := 0;
  must_replay := List.rev_append (List.rev !must_replay) !replay_events;
  replay_events := []

let now () = Int32.to_float (Sdl.get_ticks ()) /. 1000.0
let max_refresh_time = 4. /. 60.

let draw_progress ~io () =
  let count_done =
    List.fold_left (fun acc (_, c) -> acc + c) 0 !replay_events
  in
  let count_todo = List.fold_left (fun acc (_, c) -> acc + c) 0 !must_replay in
  assert (count_done = !clock);
  let total = count_done + count_todo in
  let percent = if total = 0 then 1.0 else float count_done /. float total in
  let width = Box2.w (Window.box ~io) in
  Gamelle_common.z ~io @@ fun ~io ->
  Draw.fill_rect ~io ~color:Color.black (Box2.v V2.zero (Size2.v width 50.0));
  Draw.fill_rect ~io ~color:Color.red
    (Box2.v (V2.v 10.0 10.0) (Size2.v (percent *. (width -. 20.0)) 30.0))

let replay ~backend ~events ~latest_io current_run =
  let lst = !must_replay in
  if lst = [] then false
  else
    match !current_run with
    | No_run -> invalid_arg "No game currently running"
    | Run { state; update; clean } ->
        let t0 = now () in
        let to_clean = ref clean in
        let rec go ~state = function
          | [] ->
              must_replay := [];
              state
          | (_, 0) :: es -> go ~state es
          | rest when now () -. t0 > max_refresh_time ->
              must_replay := List.rev rest;
              draw_progress ~io:!latest_io ();
              state
          | (e, count) :: es ->
              let e = { e with clock = !clock } in
              events := e;
              add e;
              let io = { (Gamelle_common.make_io backend) with event = e } in
              latest_io := io;
              let state = update ~io state in
              to_clean := List.rev_append !(io.clean) !to_clean;
              go ~state ((e, count - 1) :: es)
        in
        let state = go ~state (List.rev lst) in
        current_run := Run { state; update; clean = !to_clean };
        true
