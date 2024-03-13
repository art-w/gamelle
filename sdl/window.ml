open Common
open Gamelle_geometry

let set_size (w, h) = Sdl.set_window_size (Option.get !Global.window) ~w ~h

let size () =
  let x, y = Sdl.get_window_size (Option.get !Global.window) in
  Size2.v (float x) (float y)

let box () = Box2.v V2.zero (size ())
