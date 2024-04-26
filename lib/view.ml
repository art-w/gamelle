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

let drawing_box ?(scale = false) ?(set_window_size = true) box io =
  let ratio_w =
    let w_win = Size.w (Window.size ~io) in
    let w_box = Box.w box in
    w_win /. w_box
  in
  let ratio_h =
    let h_win = Size.h (Window.size ~io) in
    let h_box = Box.h box in
    h_win /. h_box
  in
  let ratio = Float.min ratio_h ratio_w in
  let io = if scale then scaled ratio io else io in
  let io_scale = io.view.scale in
  let size = Vec.(1. /. io_scale * Box.size box) in
  if set_window_size && (not scale) && !previous_size <> size then (
    (* If you repeatdly set the size of a maximised window, it could lead to
       very big performance issue. This also has the benefit how allowing the
       user to resize their windows. *)
    previous_size := size;
    Window.set_size ~io (Box.size box));

  let window_mid = Box.mid (Window.box ~io) in
  let box_mid = Box.mid box in
  let tr = Vec.((1. /. io_scale * window_mid) - box_mid) in
  let io = { io with centering_translation = tr } in
  let io = translated tr io in
  io
