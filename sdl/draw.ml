open Common
open Gamelle_geometry
module Gfx = Tsdl_gfx.Gfx
module Io = Gamelle_common.Io
module Delayed = Gamelle_common.Delayed

type io = Io.t

let set_color c =
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  (* Format.printf "%i %i %i %i@." r g b a ; *)
  let& () = Sdl.set_render_draw_color (render ()) r g b a in
  ()

let tau = 8.0 *. atan 1.0
let point_zero = Sdl.Point.create ~x:0 ~y:0

let project ~io p =
  let x, y = V2.to_tuple (Io.project ~io p) in
  (int x, int y)

let draw_clip ~io renderer f =
  let clip = io.Io.clip in
  let* () =
    Option.map
      (fun clip ->
        let clip = Box.translate clip io.Io.centering_translation in
        let clip_rect =
          Sdl.Rect.create
            ~x:(int @@ Box.minx clip)
            ~y:(int @@ Box.miny clip)
            ~w:(int @@ Box.w clip)
            ~h:(int @@ Box.h clip)
        in
        Sdl.render_set_clip_rect renderer (Some clip_rect))
      clip
    |> function
    | None -> Ok ()
    | Some v -> v
  in
  let r = f () in
  let* () =
    if Option.is_some clip then
      let win = Window.box () in
      let clip_rect =
        Sdl.Rect.create
          ~x:(int @@ Box.minx win)
          ~y:(int @@ Box.miny win)
          ~w:(int @@ Box.w win)
          ~h:(int @@ Box.h win)
      in
      Sdl.render_set_clip_rect renderer (Some clip_rect)
    else Ok ()
  in
  r

let draw ~io bmp p =
  let bmp = Delayed.force ~io bmp in
  let scale = io.Io.view.scale in
  let x, y = project ~io p in
  let w, h = Bitmap.size bmp in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in

  let dst =
    Sdl.Rect.create ~x ~y
      ~w:(int (scale *. float w))
      ~h:(int (scale *. float h))
  in

  let open Bitmap in
  let angle = io.Io.view.rotate *. 360.0 /. tau in
  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () ->
        Sdl.render_copy_ex ~src ~dst renderer bmp.bmp angle (Some point_zero)
          Sdl.Flip.none)
  in
  ()

let draw_string ~io ~color font ~size text p =
  let bitmap = Font.draw ~color font size text in
  draw ~io bitmap p;
  Bitmap.free ~io bitmap

let text_size ~io font ~size text =
  Delayed.force ~io @@ Font.text_size font size text

let draw_line ~io ~color segment =
  let p, p' = Segment.to_tuple segment in
  set_color color;
  let x0, y0 = project ~io p in
  let x1, y1 = project ~io p' in

  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () -> Sdl.render_draw_line renderer x0 y0 x1 y1)
  in
  ()

let draw_rect ~io ~color rect =
  draw_line ~io ~color (Box.top rect);
  draw_line ~io ~color (Box.bottom rect);
  draw_line ~io ~color (Box.left rect);
  draw_line ~io ~color (Box.right rect)

let draw_poly ~io ~color arr =
  set_color color;
  let arr = List.map (project ~io) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () ->
        Gfx.polygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_poly ~io ~color arr =
  set_color color;
  let arr = List.map (project ~io) arr in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () ->
        Gfx.filled_polygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_rect ~io ~color rect =
  set_color color;
  let p0 = Box2.tl_pt rect
  and p1 = Box2.tr_pt rect
  and p2 = Box2.br_pt rect
  and p3 = Box2.bl_pt rect in
  let pts = [ p0; p1; p2; p3 ] in
  fill_poly ~io ~color pts

let draw_circle ~io ~color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  set_color color;
  let x, y = project ~io center in
  let radius = int (io.Io.view.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () ->
        Gfx.circle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()

let fill_circle ~io ~color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  set_color color;
  let x, y = project ~io center in
  let radius = int (io.Io.view.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color (render ()) in
  let& () =
    let renderer = render () in
    draw_clip ~io renderer (fun () ->
        Gfx.filled_circle_rgba (render ()) ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()

let show_cursor b =
  let& _ = Sdl.show_cursor b in
  ()
