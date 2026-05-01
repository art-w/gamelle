open Common
open Gamelle_common
open Geometry
module Delayed = Gamelle_common.Delayed

let get_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  to_raylib_color c

let v2 x y = Raylib.Vector2.create x y



let tau = 8.0 *. atan 1.0

let draw ~io bmp p =
  let bmp = Delayed.force ~io bmp in
  let scale = io.view.Transform.scale in
  let x, y = project ~io p in
  let src =
    Raylib.Rectangle.create bmp.Bitmap.src_x bmp.Bitmap.src_y
      (float bmp.Bitmap.w) (float bmp.Bitmap.h)
  in
  let dst =
    Raylib.Rectangle.create x y
      (float bmp.Bitmap.w *. scale)
      (float bmp.Bitmap.h *. scale)
  in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  with_scissor ~io @@ fun () ->
  Raylib.draw_texture_pro bmp.Bitmap.texture src dst (v2 0. 0.) angle
    Raylib.Color.white

let draw_line ~io ?color segment =
  let p0, p1 = Segment.to_tuple segment in
  let x0, y0 = project ~io p0 in
  let x1, y1 = project ~io p1 in
  let color = get_color ~io color in
  with_scissor ~io @@ fun () ->
  Raylib.draw_line_ex (v2 x0 y0) (v2 x1 y1) 1. color

let project_points ~io pts =
  let n = List.length pts in
  let arr = Raylib.CArray.make Raylib.Vector2.t n in
  List.iteri
    (fun i p ->
      let x, y = project ~io p in
      Raylib.CArray.set arr i (v2 x y))
    pts;
  (arr, n)

let draw_poly ~io ?color poly =
  let pts = Polygon.points poly in
  let n = List.length pts in
  if n >= 2 then
    let color = get_color ~io color in
    let arr = Raylib.CArray.make Raylib.Vector2.t (n + 1) in
    List.iteri
      (fun i p ->
        let x, y = project ~io p in
        Raylib.CArray.set arr i (v2 x y))
      pts;
    let x, y = project ~io (List.hd pts) in
    Raylib.CArray.set arr n (v2 x y);
    with_scissor ~io @@ fun () ->
    Raylib.draw_line_strip (Raylib.CArray.start arr) (n + 1) color

let fill_poly ~io ?color poly =
  let pts = Polygon.points poly in
  let n = List.length pts in
  if n >= 3 then
    let color = get_color ~io color in
    let arr, n = project_points ~io pts in
    with_scissor ~io @@ fun () ->
    Raylib.draw_triangle_fan (Raylib.CArray.start arr) n color

let draw_rect ~io ?color rect =
  draw_poly ~io ?color
    (Polygon.v
       [
         Box.top_left rect;
         Box.top_right rect;
         Box.bottom_right rect;
         Box.bottom_left rect;
       ])

let fill_rect ~io ?color rect =
  with_scissor ~io @@ fun () ->
   Raylib.draw_rectangle
    (int_of_float (Box.x_left rect))
    (int_of_float (Box.y_top rect))
    (int_of_float (Box.width rect))
    (int_of_float (Box.height rect))
    (get_color ~io color)

let draw_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  with_scissor ~io @@ fun () ->
  Raylib.draw_circle_lines_v (v2 x y) radius color

let fill_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  with_scissor ~io @@ fun () ->
  Raylib.draw_circle_v (v2 x y) radius color
