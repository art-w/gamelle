include Common
module Geometry = Gamelle_common.Geometry
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
module Text = Text
include Draw
module Tsdl_image = Tsdl_image.Image
module Ttf = Tsdl_ttf.Ttf
module Window = Window

let lock = Mutex.create ()
let current_run = ref No_run
let desired_time = 1.0 /. 60.0

let run () =
  let& () = Sdl.init Sdl.Init.(video + audio) in
  Sdl.start_text_input ();
  let _ = Tsdl_image.init Tsdl_image.Init.(jpg + png) in
  let& _ = Tsdl_mixer.Mixer.init Tsdl_mixer.Mixer.Init.(ogg + mp3) in
  let& _ = Ttf.init () in

  let _ok : bool =
    Tsdl.Sdl.set_hint Tsdl.Sdl.Hint.render_scale_quality "nearest"
  in
  let _ok : bool = Tsdl.Sdl.set_hint Tsdl.Sdl.Hint.render_vsync "1" in

  let& () =
    Tsdl_mixer.Mixer.open_audio 44_100 Tsdl_mixer.Mixer.default_format 2 2048
  in

  ignore @@ Tsdl_mixer.Mixer.allocate_channels Gamelle_common.max_sounds;

  let& window =
    Sdl.create_window "Test" ~w:640 ~h:480 Sdl.Window.(windowed + resizable)
  in
  let& renderer =
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) window
  in

  let backend =
    { window; renderer; font = Font.default; font_size = Font.default_size }
  in

  let& _ = Sdl.show_cursor false in

  let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
  let start_time = t0 in
  let now_prev = ref t0 in
  let now = ref t0 in

  let open Gamelle_common in
  let events = ref Events_backend.default in

  let latest_io = ref (make_io backend) in

  let rec loop () : unit =
    let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let t0 = t0 -. start_time in
    now_prev := !now;
    now := t0;

    let was_replayed =
      mutex_protect lock @@ fun () ->
      Replay.replay ~backend ~events ~latest_io current_run
    in

    let previous = !events in
    events := Events_sdl.reset ~now:!Replay.clock !events;
    Sdl.pump_events ();
    let has_focus =
      (not was_replayed)
      &&
      let flags = Sdl.get_window_flags window in
      Sdl.Window.(test input_focus) flags
    in
    if has_focus then (
      let e = Sdl.Event.create () in
      while Sdl.poll_event (Some e) do
        events := Events_sdl.update !events e;
        events := Events_sdl.update_mouse !events
      done;
      events := Events_backend.update_updown previous !events)
    else if not was_replayed then (
      Sdl.flush_events min_int max_int;
      let& () = Sdl.wait_event None in
      ());

    mutex_protect lock (fun () ->
        match !current_run with
        | No_run -> invalid_arg "No game currently running"
        | Run { state; update; clean } when has_focus ->
            Replay.add !events;
            let io = { (make_io backend) with event = !events } in
            latest_io := io;
            let state = update ~io state in
            Window.finalize_set_size ();
            let clean = List.rev_append !(io.clean) clean in
            current_run := Run { state; update; clean }
        | Run _ -> ());

    (let io = !latest_io in
     let draw_calls = !(io.draws) in
     fill_rect ~io ~color:Geometry.Color.black (Window.box ~io);
     Gamelle_common.finalize_frame ~io;
     io.draws := draw_calls;
     Sdl.render_present renderer);

    if Events_backend.is_pressed !events `quit then raise Exit;

    let now = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let frame_elapsed = now -. t0 in
    let wait_time =
      Int32.of_float (max 0.001 (1000.0 *. (desired_time -. frame_elapsed)))
    in
    Sdl.delay wait_time;

    loop ()
  in
  (try loop () with Exit -> ());
  (match !current_run with
  | No_run -> assert false
  | Run { clean; _ } ->
      List.iter (fun fn -> fn ()) clean;
      current_run := No_run);
  Tsdl_mixer.Mixer.close_audio ();
  Tsdl_mixer.Mixer.quit ();
  Tsdl_image.quit ();
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window

let run state update =
  Mutex.lock lock;
  let prev = !current_run in
  current_run := Run { state; update; clean = [] };
  match prev with
  | No_run ->
      Mutex.unlock lock;
      run ()
  | Run { clean; _ } ->
      List.iter (fun fn -> fn ()) clean;
      Replay.reload ();
      Mutex.unlock lock
