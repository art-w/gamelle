open Gamelle_common
open Geometry
open Common

let current_size = ref (0, 0)
let get_size ~io = !(io.window_size)

let set_size ~io =
  let w, h = get_size ~io in
  let s = (w, h + int_of_float !Gamelle_common.ui_replay_height) in
  if s <> !current_size then (
    let w, h = s in
    Sdl.set_window_size io.backend.window ~w ~h;
    current_size := s)

let box ~io =
  let w, h = get_size ~io in
  Box.v Vec.zero (Size.v (float w) (float h))

let show_cursor ~io:_ b =
  let& _ = Sdl.show_cursor b in
  ()

let finalize_frame ~io =
  set_size ~io;
  let& () = Sdl.set_render_draw_color io.backend.renderer 0 0 0 255 in
  let& () = Sdl.render_clear io.backend.renderer in
  Gamelle_common.finalize_frame ~io

let has_focus window =
  let flags = Sdl.get_window_flags window in
  Sdl.Window.(test input_focus) flags
