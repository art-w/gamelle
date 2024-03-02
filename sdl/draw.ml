open Common
open Gg
module Gfx = Tsdl_gfx.Gfx
module V = Gamelle_common.View

let set_color c =
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  (* Format.printf "%i %i %i %i@." r g b a ; *)
  let& () = Sdl.set_render_draw_color (render ()) r g b a in
  ()

let tau = 8.0 *. atan 1.0
let point_zero = Sdl.Point.create ~x:0 ~y:0

let project ~view p =
  let x, y = Gamelle_common.View.project ~view p in
  (int x, int y)

let draw ~view (lazy bmp) p =
  let scale = view.V.scale in
  let x, y = project ~view p in
  let w, h = Bitmap.size bmp in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in
  let dst =
    Sdl.Rect.create ~x ~y
      ~w:(int (scale *. float w))
      ~h:(int (scale *. float h))
  in
  let open Bitmap in
  let angle = view.V.rotate *. 360.0 /. tau in
  let& () =
    Sdl.render_copy_ex ~src ~dst (render ()) bmp.bmp angle (Some point_zero)
      Sdl.Flip.none
  in
  ()

let draw_string ~view ~color font ~size text p =
  set_color color;
  draw ~view (Font.draw font size text) p

let draw_line ~view ~color p p' =
  set_color color;
  let x0, y0 = project ~view p in
  let x1, y1 = project ~view p' in
  let& () = Sdl.render_draw_line (render ()) x0 y0 x1 y1 in
  ()

let draw_rect ~view ~color rect =
  let p0 = Box2.tl_pt rect
  and p1 = Box2.tr_pt rect
  and p2 = Box2.br_pt rect
  and p3 = Box2.bl_pt rect in
  draw_line ~view ~color p0 p1;
  draw_line ~view ~color p1 p2;
  draw_line ~view ~color p2 p3;
  draw_line ~view ~color p3 p0

let draw_poly ~view ~color arr =
  set_color color;
  let arr = List.map (project ~view) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let fill_poly ~view ~color arr =
  set_color color;
  let arr = List.map (project ~view) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let fill_rect ~view ~color rect =
  set_color color;
  let p0 = Box2.tl_pt rect
  and p1 = Box2.tr_pt rect
  and p2 = Box2.br_pt rect
  and p3 = Box2.bl_pt rect in
  let pts = [ p0; p1; p2; p3 ] in
  fill_poly ~view ~color pts

let draw_circle ~view ~color center radius =
  set_color color;
  let x, y = project ~view center in
  let radius = int (view.V.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let fill_circle ~view ~color center radius =
  set_color color;
  let x, y = project ~view center in
  let radius = int (view.V.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let show_cursor b =
  let& _ = Sdl.show_cursor b in
  ()
