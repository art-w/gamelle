open Gamelle_common
open Draw_geometry
include Gamelle_common.View

let font ft io =
  { io with backend = Gamelle_backend.Font.set_font ft io.backend }

let font_size s io =
  { io with backend = Gamelle_backend.Font.set_font_size s io.backend }

let project ~io p = Transform.project io.view p

let drawing_box ?scale:must_scale ?(set_window_size = true) box io =
  let must_scale = Option.value must_scale ~default:false in
  let window_size = Window_.size ~io in
  let ratio_w =
    let w_win = Size.width window_size in
    let w_box = Box.width box in
    w_win /. w_box
  in
  let ratio_h =
    let h_win = Size.height window_size in
    let h_box = Box.height box in
    h_win /. h_box
  in
  let ratio = Float.min ratio_h ratio_w in
  let io = if must_scale then scale ratio io else io in
  let io_scale = io.view.scale in
  let size = Box.size box in
  if set_window_size && not must_scale then Window_.set_size ~io size;
  let window_mid = Box.center (Window_.box ~io) in
  let box_mid = Box.center box in
  let inv_scale = 1. /. io_scale in
  let tr = Vec.((inv_scale * window_mid) - box_mid) in
  let io = translate tr io in
  io
