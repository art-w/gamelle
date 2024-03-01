open Common
module Gfx = Tsdl_gfx.Gfx

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

let draw_string font ~size text x y = draw (Font.draw font size text) x y

let get_color () =
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor a

let set_color c =
  let r = (c lsr 24) land 0xFF in
  let g = (c lsr 16) land 0xFF in
  let b = (c lsr 8) land 0xFF in
  let a = c land 0xFF in
  (* Format.printf "%i %i %i %i@." r g b a ; *)
  let& () = Sdl.set_render_draw_color (render ()) r g b a in
  ()

let draw_line (x0, y0) (x1, y1) =
  let& () =
    Sdl.render_draw_line (render ()) (int x0) (int y0) (int x1) (int y1)
  in
  ()

let draw_rect (x, y) (w, h) =
  let x, y, w, h = (int x, int y, int w, int h) in
  let& () =
    Sdl.render_draw_rect (render ()) (Some (Sdl.Rect.create ~x ~y ~w ~h))
  in
  ()

let fill_rect (x, y) (w, h) =
  let x, y, w, h = (int x, int y, int w, int h) in
  let& () =
    Sdl.render_fill_rect (render ()) (Some (Sdl.Rect.create ~x ~y ~w ~h))
  in
  ()

let draw_poly arr =
  let arr = List.map (fun (x, y) -> (int x, int y)) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let fill_poly arr =
  let arr = List.map (fun (x, y) -> (int x, int y)) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_polygon_rgba (render ()) ~ps:arr ~r ~g ~b ~a in
  ()

let draw_thick_line ~stroke (x1, y1) (x2, y2) =
  let stroke = int stroke in
  let x1, y1, x2, y2 = (int x1, int y1, int x2, int y2) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    Gfx.thick_line_rgba (render ()) ~x1 ~y1 ~x2 ~y2 ~width:stroke ~r ~g ~b ~a
  in
  ()

let draw_circle (x, y) radius =
  let x, y, radius = (int x, int y, int radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let fill_circle (x, y) radius =
  let x, y, radius = (int x, int y, int radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () = Gfx.filled_circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a in
  ()

let show_cursor b =
  let& _ = Sdl.show_cursor b in
()