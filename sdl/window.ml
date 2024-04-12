open Common
open Gamelle_common.Geometry
let set_size s =
  let w = s |> Size.w |> int_of_float and h = s |> Size.h |> int_of_float in
  Sdl.set_window_size (Option.get !Global.window) ~w ~h

let size () =
  let x, y = Sdl.get_window_size (Option.get !Global.window) in
  Size.v (float x) (float y)

let box () = Box.v Vec.zero (size ())
