open Common
open Gg
module Color = Color
module Bitmap = Bitmap
module Font = Font
module Sound = Sound
module Event = Event
module View = Gamelle_common.View
include Draw

let clock = Common.clock
let dt = Common.dt

module Ttf = Tsdl_ttf

let global_window = ref None

let window_size () =
  let x, y = Sdl.get_window_size (Option.get !global_window) in
  Size2.v (float x) (float y)

let window_box () = Box2.v V2.zero (window_size ())

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : Event.t -> 'a -> 'a;
      render : view:View.t -> 'a -> unit;
      on_exit : 'a -> unit;
    }
      -> run

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
  global_window := Some window;
  let& renderer =
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) window
  in
  Common.set_render renderer;

  let& _ = Sdl.show_cursor false in

  let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
  Common.start_time := t0;
  Common.now := t0;

  let events = ref Event.default in

  let rec loop () : unit =
    let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    Common.now_prev := !Common.now;
    Common.now := t0;
    let e = Sdl.Event.create () in
    events := Event.reset !events;
    while Sdl.poll_event (Some e) do
      events := Event.update !events e;
      events := Event.update_mouse !events
    done;

    (* Format.printf "playing: %b@." (Tsdl_mixer.Mixer.playing (Some 1)) ; *)
    let& () = Sdl.render_clear renderer in

    (match !current_run with
    | No_run -> invalid_arg "No game currently running"
    | Run { state; update; render; on_exit } ->
        let state = update !events state in
        render ~view:View.default state;
        current_run := Run { state; update; render; on_exit });

    Sdl.render_present renderer;
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

  Tsdl_mixer.Mixer.close_audio ();
  Tsdl_mixer.Mixer.quit ();
  Tsdl_image.quit ();
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window

let cleanup () =
  match !current_run with
  | No_run -> assert false
  | Run { state; on_exit; _ } -> on_exit state

let run ?(on_exit = ignore) state ~update ~render =
  match !current_run with
  | No_run ->
      current_run := Run { state; update; render; on_exit };
      run ();
      cleanup ()
  | Run _ ->
      Format.printf "set new run@.";
      current_run := Run { state; update; render; on_exit }
