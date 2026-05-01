open Gamelle_common
open Geometry

let current_size = ref (0, 0)

let set_size ~io =
  let w, h = !(io.window_size) in
  if (w, h) <> (0, 0) && (w, h) <> !current_size then begin
    Raylib.set_window_size w h;
    current_size := (w, h)
  end

let size ~io:_ =
  let w = Raylib.get_screen_width () in
  let h = Raylib.get_screen_height () in
  Size.v (float w) (float h)

let show_cursor ~io:_ show =
  if show then Raylib.show_cursor () else Raylib.hide_cursor ()

let finalize_frame ~io =
  set_size ~io;
  Raylib.clear_background (Raylib.Color.create 0 0 0 255);
  Gamelle_common.finalize_frame ~io
