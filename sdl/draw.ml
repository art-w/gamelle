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

let project ~view (x, y) =
  let { Gamelle_common.View.scale; translate = dx, dy; rotate = angle } =
    view
  in
  let c, s = (scale *. cos angle, scale *. sin angle) in
  let x, y = ((c *. x) -. (s *. y), (s *. x) +. (c *. y)) in
  (int (x +. dx), int (y +. dy))

let draw ~view (lazy bmp) x y =
  let scale = view.V.scale in
  let x, y = project ~view (x, y) in
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

let draw_string ~view ~color font ~size text x y =
  set_color color;
  draw ~view (Font.draw font size text) x y

let draw_line ~view ~color (x0, y0) (x1, y1) =
  set_color color;
  let x0, y0 = project ~view (x0, y0) in
  let x1, y1 = project ~view (x1, y1) in
  let& () = Sdl.render_draw_line (render ()) x0 y0 x1 y1 in
  ()

let draw_rect ~view ~color (x, y) (w, h) =
  let p1 = (x +. w, y) in
  let p2 = (x +. w, y +. h) in
  let p3 = (x, y +. h) in
  draw_line ~view ~color (x, y) p1;
  draw_line ~view ~color p1 p2;
  draw_line ~view ~color p2 p3;
  draw_line ~view ~color p3 (x, y)

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

let fill_rect ~view ~color (x, y) (w, h) =
  let pts = [ (x, y); (x +. w, y); (x +. w, y +. h); (x, y +. h) ] in
  fill_poly ~view ~color pts

let draw_circle ~view ~color (x, y) radius =
  set_color color;
  let x, y = project ~view (x, y) in
  let radius = int (view.V.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let fill_circle ~view ~color (x, y) radius =
  set_color color;
  let x, y = project ~view (x, y) in
  let radius = int (view.V.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let show_cursor b =
  let& _ = Sdl.show_cursor b in
  ()
