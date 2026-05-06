include Common
module Geometry = Gamelle_common.Geometry
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
module Text = Text
include Draw
module Window = Window

let run state update =
  Raylib.set_config_flags [ Raylib.ConfigFlags.Msaa_4x_hint; Raylib.ConfigFlags.Window_highdpi ] ;
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  Raylib.init_window 640 640 "Gamelle";
  Raylib.set_target_fps 60;
  Raylib.init_audio_device ();

  let backend = { font = Font_.default; font_size = Font_.default_size } in
  let io = Gamelle_common.make_io backend in
  let dpi_scale = Raylib.Vector2.x (Raylib.get_window_scale_dpi ()) in
  let clock_ref = ref 0 in
  let state = ref state in

  while not (Raylib.window_should_close ()) do
    Raylib.begin_drawing ();
    let prev_event = !(io.event) in
    Gamelle_common.io_reset_mutable_fields io;
    io.event := Events_raylib.update !clock_ref prev_event;
    incr clock_ref;
    let io = { io with view = Transform.scale dpi_scale io.view } in
    state := update ~io !state;
    Window.finalize_frame ~io;
    Sound.update_current_music ();
    Raylib.end_drawing ()
  done;

  Sound.cleanup ();
  Raylib.close_audio_device ();
  Raylib.close_window ()
