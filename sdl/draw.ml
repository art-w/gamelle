open Common
open Gg
module Gfx = Tsdl_gfx.Gfx

let set_color c =
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  (* Format.printf "%i %i %i %i@." r g b a ; *)
  let& () = Sdl.set_render_draw_color (render ()) r g b a in
  ()

let draw (lazy bmp) x y =
  let x, y = (int x, int y) in
  let bmp_w, bmp_h = Bitmap.bmp_size bmp in
  let w, h = Bitmap.size bmp in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:bmp_w ~h:bmp_h in
  let dst = Sdl.Rect.create ~x ~y ~w ~h in
  let open Bitmap in
  let& () =
    Sdl.render_copy_ex ~src ~dst (render ()) bmp.bmp bmp.angle None
      Sdl.Flip.none
  in
  ()

let draw_string ~color font ~size text x y =
  set_color color;
  draw (Font.draw font size text) x y

let draw_line ~color (x0, y0) (x1, y1) =
  set_color color;

  let& () =
    Sdl.render_draw_line (render ()) (int x0) (int y0) (int x1) (int y1)
  in
  ()

let draw_rect ~color (x, y) (w, h) =
  set_color color;

  let x, y, w, h = (int x, int y, int w, int h) in
  let& () =
    Sdl.render_draw_rect (render ()) (Some (Sdl.Rect.create ~x ~y ~w ~h))
  in
  ()

let fill_rect ~color (x, y) (w, h) =
  set_color color;

  let x, y, w, h = (int x, int y, int w, int h) in
  let& () =
    Sdl.render_fill_rect (render ()) (Some (Sdl.Rect.create ~x ~y ~w ~h))
  in
  ()

let draw_poly ~color arr =
  set_color color;

  let arr = List.map (fun (x, y) -> (int x, int y)) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let fill_poly ~color arr =
  set_color color;

  let arr = List.map (fun (x, y) -> (int x, int y)) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let draw_thick_line ~color ~stroke (x1, y1) (x2, y2) =
  set_color color;

  let stroke = int stroke in
  let x1, y1, x2, y2 = (int x1, int y1, int x2, int y2) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    Gfx.thick_line_rgba (render ()) ~x1 ~y1 ~x2 ~y2 ~width:stroke ~r ~g ~b ~a
  in
  ()

let draw_circle ~color (x, y) radius =
  set_color color;

  let x, y, radius = (int x, int y, int radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let fill_circle ~color (x, y) radius =
  set_color color;
  let x, y, radius = (int x, int y, int radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let show_cursor b =
  let& _ = Sdl.show_cursor b in
  ()
