open Common
open Gamelle_geometry

let set_size s =
  let w = s |> Size2.w |> int_of_float and h = s |> Size2.h |> int_of_float in
  Sdl.set_window_size (Option.get !Global.window) ~w ~h

let size () =
  let x, y = Sdl.get_window_size (Option.get !Global.window) in
  Size2.v (float x) (float y)

let box () = Box2.v V2.zero (size ())
