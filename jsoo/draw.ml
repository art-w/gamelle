open Gg
open Common
module V = Gamelle_common.View

let set_color c =
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  let color =
    C.color (Jstr.of_string (Printf.sprintf "rgba(%i,%i,%i,%i)" r g b a))
  in
  C.set_fill_style (render ()) color;
  C.set_stroke_style (render ()) color

let transform ~view =
  C.reset_transform (render ());
  let dx, dy = view.V.translate in
  C.translate (render ()) ~x:dx ~y:dy;
  C.rotate (render ()) view.V.rotate;
  C.scale (render ()) ~sx:view.V.scale ~sy:view.V.scale

let draw ~view bmp p =
  transform ~view;
  let x, y = V2.to_tuple p in
  Bitmap.draw ~view ~ctx:(render ()) bmp ~x ~y

let fill_rect ~view ~color rect =
  transform ~view;
  let x, y = V2.to_tuple (Box2.o rect) in
  let w, h = V2.to_tuple (Box2.size rect) in
  set_color color;
  C.fill_rect (render ()) ~x ~y ~w ~h

let draw_rect ~view ~color rect =
  transform ~view;
  let x, y = V2.to_tuple (Box2.o rect) in
  let w, h = V2.to_tuple (Box2.size rect) in
  set_color color;
  C.stroke_rect (render ()) ~x ~y ~w ~h

let draw_line ~view ~color p0 p1 =
  transform ~view;
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

let draw_poly ~view ~color pts =
  transform ~view;
  set_color color;
  let path = path_poly pts in
  C.stroke (render ()) path

let fill_poly ~view ~color pts =
  transform ~view;
  set_color color;
  let path = path_poly pts in
  C.fill (render ()) path

let tau = 8.0 *. atan 1.0

let draw_circle ~view ~color center radius =
  transform ~view;
  set_color color;
  let x, y = V2.to_tuple center in
  let path = C.Path.create () in
  C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
  C.stroke (render ()) path

let fill_circle ~view ~color center radius =
  transform ~view;
  set_color color;
  let x, y = V2.to_tuple center in
  let path = C.Path.create () in
  C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
  C.fill (render ()) path

let draw_string ~view:_ ~color:_ font ~size txt p =
  let x, y = V2.to_tuple p in
  Font.draw_at font ~size txt (x, y)

let show_cursor status =
  match !Common.global_canvas with
  | None -> ()
  | Some t ->
      let el = Brr_canvas.Canvas.to_el t in
      if status then
        Brr.El.remove_inline_style Brr.El.Style.cursor (Common.Window.to_el t)
      else
        Brr.El.set_inline_style Brr.El.Style.cursor (Jstr.of_string "none") el
