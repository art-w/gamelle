open Gamelle_common
open Geometry
open Common

let current_size = ref (0, 0)

let set_size ~io =
  let s = !(io.window_size) in
  if s <> !current_size then (
    let w, h = s in
    Sdl.set_window_size io.backend.window ~w ~h;
    current_size := s)

let box ~io =
  let w, h = !(io.window_size) in
  Box.v Vec.zero (Size.v (float w) (float h))

let show_cursor ~io:_ b =
  let& _ = Sdl.show_cursor b in
  ()

let finalize_frame ~io =
  set_size ~io;
  let& () = Sdl.set_render_draw_color io.backend.renderer 0 0 0 255 in
  let& () = Sdl.render_clear io.backend.renderer in
  Gamelle_common.finalize_frame ~io
