let () = Random.init 0

module Ui = Ui
module Physics = Physics
module View = View
module Transform = Gamelle_common.Transform
module Input = Event
include Draw_geometry

let dt = Gamelle_backend.dt
let clock = Gamelle_backend.clock

module Sound = Gamelle_backend.Sound
module Window = Gamelle_backend.Window

let run init f =
  Gamelle_backend.run init (fun ~io ->
      let r = f ~io in
      Gamelle_common.finalize_frame ~io;
      r)

module Font = Gamelle_backend.Font

type io = Gamelle_backend.io

module Bitmap = Gamelle_backend.Bitmap

let draw_string ~io ?color ?font ?size ~at txt =
  Text.draw ~io ?color ?font ?size ~at txt
