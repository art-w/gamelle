open Gamelle_geometry
open Common
module Io = Gamelle_common.Io

type io = Io.t

let set_color c =
  let r, g, b, a = Color.to_srgbi c in
  let color =
    C.color (Jstr.of_string (Printf.sprintf "rgba(%i,%i,%i,%f)" r g b a))
  in
  C.set_fill_style (render ()) color;
  C.set_stroke_style (render ()) color

let transform ~io:{ Io.view; _ } =
  C.reset_transform (render ());
  let dx, dy = V2.to_tuple view.translate in
  C.translate (render ()) ~x:dx ~y:dy;
  C.rotate (render ()) view.rotate;
  C.scale (render ()) ~sx:view.scale ~sy:view.scale

let draw ~io bmp p =
  transform ~io;
  let x, y = V2.to_tuple p in
  Bitmap.draw ~io ~ctx:(render ()) bmp ~x ~y

let fill_rect ~io ~color rect =
  transform ~io;
  let x, y = V2.to_tuple (Box2.o rect) in
  let w, h = V2.to_tuple (Box2.size rect) in
  set_color color;
  C.fill_rect (render ()) ~x ~y ~w ~h

let draw_rect ~io ~color rect =
  transform ~io;
  let x, y = V2.to_tuple (Box2.o rect) in
  let w, h = V2.to_tuple (Box2.size rect) in
  set_color color;
  C.stroke_rect (render ()) ~x ~y ~w ~h

let draw_line ~io ~color segment =
  let p0, p1 = Segment.to_tuple segment in
  transform ~io;
  let x0, y0 = V2.to_tuple p0 in
  let x1, y1 = V2.to_tuple p1 in
  set_color color;
  let path = C.Path.create () in
  C.Path.move_to path ~x:x0 ~y:y0;
  C.Path.line_to path ~x:x1 ~y:y1;
  C.stroke (render ()) path

let path_poly pts =
  let path = C.Path.create () in
  List.iter
    (fun pt ->
      let x, y = V2.to_tuple pt in
      C.Path.line_to path ~x ~y)
    pts;
  C.Path.close path;
  path

let draw_poly ~io ~color pts =
  transform ~io;
  set_color color;
  let path = path_poly pts in
  C.stroke (render ()) path

let fill_poly ~io ~color pts =
  transform ~io;
  set_color color;
  let path = path_poly pts in
  C.fill (render ()) path

let tau = 8.0 *. atan 1.0

let draw_circle ~io ~color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  transform ~io;
  set_color color;
  let x, y = V2.to_tuple center in
  let path = C.Path.create () in
  C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
  C.stroke (render ()) path

let fill_circle ~io ~color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  transform ~io;
  set_color color;
  let x, y = V2.to_tuple center in
  let path = C.Path.create () in
  C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
  C.fill (render ()) path

let draw_string ~io ~color font ~size txt p =
  transform ~io;
  set_color color;
  let x, y = V2.to_tuple p in
  Font.draw_at font ~size txt (x, y)

let show_cursor status =
  match !Common.global_canvas with
  | None -> ()
  | Some t ->
      let el = Common.Canvas.to_el t in
      if status then Brr.El.remove_inline_style Brr.El.Style.cursor el
      else
        Brr.El.set_inline_style Brr.El.Style.cursor (Jstr.of_string "none") el
