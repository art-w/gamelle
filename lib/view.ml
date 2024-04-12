open Gamelle_geometry
open Gamelle_backend
include Gamelle_common.Io

let previous_size = ref Size2.zero

let drawing_box box io =

  let size = Box.size box in
  if !previous_size <> size then (
    (* If you repeatdly set the size of a maximised window, it could lead to
       very big performance issue. This also has the benefit how allowing the
       user to resize their windows. *)
    previous_size := size;
    Window.set_size (Box.size box));
  let tr =
    Geometry.(
      V2.(Box.(o (centered box (Window.box ()))) - io.centering_translation))
  in
  let io = { io with centering_translation = tr } in
  translated tr io
