open Gamelle_common
open Common
open Geometry

let previous_window_size = ref (Size.v (-1.) (-1.))
let size_to_be_set = ref None
let set_size ~io s = size_to_be_set := Some (io, s)

let finalize_set_size () =
  Option.iter
    (fun (io, s) ->
      let w = s |> Size.w |> int_of_float and h = s |> Size.h |> int_of_float in
      let previous = !previous_window_size in
      if previous <> s then (
        Sdl.set_window_size io.backend.window ~w ~h;
        previous_window_size := s))
    !size_to_be_set;
  size_to_be_set := None

let size ~io =
  let x, y = Sdl.get_window_size io.backend.window in
  Size.v (float x) (float y)

let box ~io = Box.v Vec.zero (size ~io)
