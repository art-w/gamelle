open Gamelle_common
open Common
open Geometry

let set_size ~io s =
  let w = s |> Size.w |> int_of_float and h = s |> Size.h |> int_of_float in
  Sdl.set_window_size io.backend.window ~w ~h

let size ~io =
  let x, y = Sdl.get_window_size io.backend.window in
  Size.v (float x) (float y)

let box ~io = Box.v Vec.zero (size ~io)
