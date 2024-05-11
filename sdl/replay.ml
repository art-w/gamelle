open Common
open! Gamelle_common.Geometry

let clock = ref 0
let target_clock = ref 0
let replay_events = ref []
let must_replay = ref []
let count_todo () = List.fold_left (fun acc (_, c) -> acc + c) 0 !must_replay

type state = int * int * int
type ui = io:io -> state -> state

let ui : ui option ref = ref None
let init fn = match !ui with None -> ui := Some fn | _ -> ()
let draw_ui ~io state = match !ui with None -> state | Some ui -> ui ~io state
let ui_state () = (!clock, !target_clock, count_todo ())

let add e =
  if !target_clock = !clock then incr target_clock;
  incr clock;
  let e = { e with Gamelle_common.Events_backend.clock = 0 } in
  replay_events :=
    match !replay_events with
    | (e', count) :: es when e' = e -> (e', count + 1) :: es
    | es -> (e, 1) :: es

let force_reload () =
  clock := 0;
  must_replay := List.rev_append (List.rev !must_replay) !replay_events;
  replay_events := []

let reload fn =
  init fn;
  force_reload ()

let now () = Int32.to_float (Sdl.get_ticks ()) /. 1000.0
let max_refresh_time = 4. /. 60.

let draw_progress ~io () =
  let _clock, new_target_clock, todos = draw_ui ~io (ui_state ()) in
  if todos = 0 then must_replay := [];
  if new_target_clock <> !target_clock then (
    target_clock := new_target_clock;
    if !target_clock < !clock then (
      force_reload ();
      State.force_reload ()))

let replay ~backend ~events ~latest_io =
  let lst = !must_replay in
  if lst = [] then `not_replayed
  else if !clock = !target_clock then `paused
  else
    let t0 = now () in
    let rec go = function
      | [] -> must_replay := []
      | (_, 0) :: es -> go es
      | rest
        when !clock >= !target_clock || State.crashed ()
             || now () -. t0 > max_refresh_time ->
          must_replay := List.rev rest
      | (e, count) :: es ->
          let e = { e with clock = !clock } in
          events := e;
          add e;
          let io =
            {
              (Gamelle_common.make_io ~previous:!latest_io backend) with
              event = e;
            }
          in
          latest_io := io;
          State.unsafe_update ~io;
          go ((e, count - 1) :: es)
    in
    go (List.rev lst);
    `replay_progress
