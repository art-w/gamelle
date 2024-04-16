open Gamelle_common
open Gamelle_backend
open Draw_geometry

let translated dxy io = { io with view = Transform.translate dxy io.view }
let scaled factor io = { io with view = Transform.scale factor io.view }
let rotated angle io = { io with view = Transform.rotate angle io.view }
let clipped clip io = { io with clip = Some clip }
let unclipped io = { io with clip = None }
let clipped_events b io = { io with clip_events = b }

type 'a scene = io:io -> 'a

let ( & ) f g ~io =
  f ~io;
  g ~io

let translate dxy fn ~io = fn ~io:(translated dxy io)
let scale factor fn ~io = fn ~io:(scaled factor io)
let rotate angle fn ~io = fn ~io:(rotated angle io)
let project ~io p = Transform.project io.view p
let clip clip fn ~io = fn ~io:(clipped clip io)
let unclip fn ~io = fn ~io:(unclipped io)
let clip_events b fn ~io = fn ~io:(clipped_events b io)
let previous_size = ref Size.zero

let drawing_box box io =
  let size = Box.size box in
  if !previous_size <> size then (
    (* If you repeatdly set the size of a maximised window, it could lead to
       very big performance issue. This also has the benefit how allowing the
       user to resize their windows. *)
    previous_size := size;
    Window.set_size ~io (Box.size box));
  let tr =
    Vec.(Box.(o (centered box (Window.box ~io))) - io.centering_translation)
  in
  let io = { io with centering_translation = tr } in
  translated tr io
