open Common
open Gamelle_common.Geometry

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
  let width = Box.width (Window.box ~io) in
  Gamelle_common.z ~io @@ fun ~io ->
  Draw.fill_rect ~io ~color:Color.black (Box.v Point.zero (Size.v width 50.0));
  Draw.fill_rect ~io ~color:Color.red
    (Box.v (Point.v 10.0 10.0) (Size.v (percent *. (width -. 20.0)) 30.0))

let replay ~events ~io =
  let lst = !must_replay in
  if lst = [] then false
  else
    let t0 = now () in
    let rec go = function
      | [] -> must_replay := []
      | (_, 0) :: es -> go es
      | rest when State.crashed () || now () -. t0 > max_refresh_time ->
          must_replay := List.rev rest
      | (e, count) :: es ->
          let e = { e with clock = !clock } in
          events := e;
          add e;
          Gamelle_common.io_reset_mutable_fields io;
          io.event := e;
          State.unsafe_update ~io;
          go ((e, count - 1) :: es)
    in
    go (List.rev lst);
    true
