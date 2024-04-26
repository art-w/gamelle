module Ui = Ui
module Physics = Physics
module View = View
module Transform = Gamelle_common.Transform
module Event = Event
include Draw_geometry

let dt = Gamelle_backend.dt
let clock = Gamelle_backend.clock

module Sound = Gamelle_backend.Sound
module Window = Gamelle_backend.Window

let run init f =
  Gamelle_backend.run init (fun ~io ->
      let r = f ~io in
      Draw_geometry.finalize_frame ();
      r)

module Font = Gamelle_backend.Font

type io = Gamelle_backend.io

module Bitmap = Gamelle_backend.Bitmap

let show_cursor = Gamelle_backend.show_cursor
