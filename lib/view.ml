open Gamelle_backend

include Gamelle_common.Io

let drawing_box box io =
  let tr =
    Geometry.(
      V2.(Box.(o (centered box (Window.box ()))) - io.centering_translation))
  in
  let io = { io with centering_translation = tr } in
  translated tr io