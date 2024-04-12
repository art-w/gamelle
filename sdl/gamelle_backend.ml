open Common
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
include Draw

let clock = Common.clock
let dt = Common.dt

module Ttf = Tsdl_ttf
module Window = Window

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : io:io -> 'a -> 'a;
      clean : (unit -> unit) list;
    }
      -> run

let lock = Mutex.create ()
let current_run = ref No_run

let run () =
  let& () = Sdl.init Sdl.Init.(video + audio) in
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

  let& window =
    Sdl.create_window "Test" ~w:640 ~h:480 Sdl.Window.(windowed + resizable)
  in
  Global.window := Some window;
  let& renderer =
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) window
  in
  Common.set_render renderer;

  let& _ = Sdl.show_cursor false in

  let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
  Common.start_time := t0;
  Common.now := t0;

  let open Gamelle_common in
  let events = ref Events_backend.default in

  let rec loop () : unit =
    let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    Common.now_prev := !Common.now;
    Common.now := t0;
    let e = Sdl.Event.create () in
    let previous = !events in
    events := Events_sdl.reset !events;
    while Sdl.poll_event (Some e) do
      events := Events_sdl.update !events e;
      events := Events_sdl.update_mouse !events
    done;
    events := Events_backend.update_updown previous !events;

    mutex_protect lock (fun () ->
        match !current_run with
        | No_run -> invalid_arg "No game currently running"
        | Run { state; update; clean } ->
            let& () = Sdl.render_clear renderer in
            let io = { (Io.make ()) with event = !events } in
            fill_rect ~io ~color:Gamelle_common.Color.black (Window.box ());
            let state = update ~io state in
            let clean = List.rev_append !(io.clean) clean in
            current_run := Run { state; update; clean });
    Sdl.render_present renderer;

    if Events_backend.is_pressed !events `quit then raise Exit;

    let now = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let frame_elapsed = now -. t0 in
    let desired_time = 1.0 /. 60.0 in
    let wait_time =
      Int32.of_float (max 0.0 (1000.0 *. (desired_time -. frame_elapsed)))
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
      Mutex.unlock lock

module Event = Gamelle_common.Io.Event
