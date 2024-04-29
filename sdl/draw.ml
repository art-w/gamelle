open Common
open Gamelle_common
open Geometry
module Gfx = Tsdl_gfx.Gfx
module Delayed = Gamelle_common.Delayed

let set_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  (* Format.printf "%i %i %i %i@." r g b a ; *)
  let& () = Sdl.set_render_draw_color io.backend.renderer r g b a in
  ()

let tau = 8.0 *. atan 1.0
let point_zero = Sdl.Point.create ~x:0 ~y:0

let project ~io p =
  let x, y = Vec.to_tuple (Transform.project io.view p) in
  (int x, int y)

let draw_clip ~io renderer f =
  let clip = io.clip in
  let* () =
    Option.map
      (fun clip ->
        let clip = Box.translate clip io.centering_translation in
        let clip_rect =
          Sdl.Rect.create
            ~x:((int @@ Box.minx clip) + 1)
            ~y:((int @@ Box.miny clip) + 1)
            ~w:((int @@ Box.w clip) - 1)
            ~h:((int @@ Box.h clip) - 1)
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
      let win = Window.box ~io in
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
  let scale = io.view.scale in
  let x, y = project ~io p in
  let w, h = Bitmap.size bmp in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in

  let dst =
    Sdl.Rect.create ~x ~y
      ~w:(int (scale *. float w))
      ~h:(int (scale *. float h))
  in

  let open Bitmap in
  let angle = io.view.rotate *. 360.0 /. tau in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Sdl.render_copy_ex ~src ~dst renderer bmp.bmp angle (Some point_zero)
          Sdl.Flip.none)
  in
  ()

let draw_line ~io ?color segment =
  let p, p' = Segment.to_tuple segment in
  set_color ~io color;
  let x0, y0 = project ~io p in
  let x1, y1 = project ~io p' in

  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () -> Sdl.render_draw_line renderer x0 y0 x1 y1)
  in
  ()

let draw_rect ~io ?color rect =
  draw_line ~io ?color (Box.top rect);
  draw_line ~io ?color (Box.bottom rect);
  draw_line ~io ?color (Box.left rect);
  draw_line ~io ?color (Box.right rect)

let draw_poly ~io ?color poly =
  set_color ~io color;
  let arr = Polygon.to_list poly in
  let arr = List.map (project ~io) arr in
  let& r, g, b, a = Sdl.get_render_draw_color io.backend.renderer in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.polygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_poly ~io ?color poly =
  set_color ~io color;
  let arr = Polygon.to_list poly in
  let arr = List.map (project ~io) arr in
  let& r, g, b, a = Sdl.get_render_draw_color io.backend.renderer in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.filled_polygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_rect ~io ?color rect =
  let p0 = Box.tl_pt rect
  and p1 = Box.tr_pt rect
  and p2 = Box.br_pt rect
  and p3 = Box.bl_pt rect in
  let pts = Polygon.v [ p0; p1; p2; p3 ] in
  fill_poly ~io ?color pts

let draw_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  set_color ~io color;
  let x, y = project ~io center in
  let radius = int (io.view.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color io.backend.renderer in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.aacircle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()

let fill_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  set_color ~io color;
  let x, y = project ~io center in
  let radius = int (io.view.scale *. radius) in
  let& r, g, b, a = Sdl.get_render_draw_color io.backend.renderer in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        let* () =
          Gfx.filled_circle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a
        in
        Gfx.aacircle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()

let show_cursor ~io:_ b =
  let& _ = Sdl.show_cursor b in
  ()
