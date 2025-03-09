let () = Random.init 0

module Ui = Ui
module Ease = Ease
module Anim = Anim
module Physics = Physics
module View = View
module Transform = Gamelle_common.Transform
module Input = Event
module Bitmap = Bitmap_
include Draw_geometry

let dt = Gamelle_backend.dt
let clock = Gamelle_backend.clock

module Sound = Gamelle_backend.Sound
module Window = Window_

let run init f =
  Gamelle_backend.run init (fun ~io ->
      Box.fill ~io ~color:Color.black (Window.box ~io);
      let r = f ~io in
      Gamelle_common.finalize_frame ~io;
      r)

module Font = Gamelle_backend.Font

type io = Gamelle_backend.io

let draw_string ~io ?color ?font ?size ~at txt =
  Text.draw ~io ?color ?font ?size ~at txt
