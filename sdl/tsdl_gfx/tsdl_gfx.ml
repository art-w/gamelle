(* https://github.com/fccm/tsdl-gfx *)

open Ctypes
open Foreign
open Tsdl
open Result

module Gfx = struct
  type 'a result = 'a Sdl.result

  let error () = Error (`Msg (Sdl.get_error ()))

  let zero_to_ok =
    let read = function 0 -> Ok () | _ -> error () in
    view ~read ~write:(fun _ -> assert false) int

  let renderer =
    view ~read:Sdl.unsafe_renderer_of_ptr ~write:Sdl.unsafe_ptr_of_renderer
      nativeint

  let pixel_rgba =
    foreign "pixelRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let pixel_rgba rnd ~x ~y ~r ~g ~b ~a = pixel_rgba rnd x y r g b a

  let hline_rgba =
    foreign "hlineRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let vline_rgba =
    foreign "vlineRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let rectangle_rgba =
    foreign "rectangleRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let hline_rgba rnd ~x1 ~x2 ~y ~r ~g ~b ~a = hline_rgba rnd x1 x2 y r g b a
  let vline_rgba rnd ~x ~y1 ~y2 ~r ~g ~b ~a = vline_rgba rnd x y1 y2 r g b a

  let rectangle_rgba rnd ~x1 ~y1 ~x2 ~y2 ~r ~g ~b ~a =
    rectangle_rgba rnd x1 y1 x2 y2 r g b a

  let line_rgba =
    foreign "lineRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let line_rgba rnd ~x1 ~y1 ~x2 ~y2 ~r ~g ~b ~a =
    line_rgba rnd x1 y1 x2 y2 r g b a

  let aaline_rgba =
    foreign "aalineRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let aaline_rgba rnd ~x1 ~y1 ~x2 ~y2 ~r ~g ~b ~a =
    aaline_rgba rnd x1 y1 x2 y2 r g b a

  let thick_line_rgba =
    foreign "thickLineRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let thick_line_rgba rnd ~x1 ~y1 ~x2 ~y2 ~width ~r ~g ~b ~a =
    thick_line_rgba rnd x1 y1 x2 y2 width r g b a

  let rounded_rectangle_rgba =
    foreign "roundedRectangleRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let rounded_rectangle_rgba rnd ~x1 ~y1 ~x2 ~y2 ~rad ~r ~g ~b ~a =
    rounded_rectangle_rgba rnd x1 y1 x2 y2 rad r g b a

  let box_rgba =
    foreign "boxRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let box_rgba rnd ~x1 ~y1 ~x2 ~y2 ~r ~g ~b ~a =
    box_rgba rnd x1 y1 x2 y2 r g b a

  let rounded_box_rgba =
    foreign "roundedBoxRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let rounded_box_rgba rnd ~x1 ~y1 ~x2 ~y2 ~rad ~r ~g ~b ~a =
    rounded_box_rgba rnd x1 y1 x2 y2 rad r g b a

  let circle_rgba =
    foreign "circleRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let circle_rgba rnd ~x ~y ~rad ~r ~g ~b ~a = circle_rgba rnd x y rad r g b a

  let aacircle_rgba =
    foreign "aacircleRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let aacircle_rgba rnd ~x ~y ~rad ~r ~g ~b ~a =
    aacircle_rgba rnd x y rad r g b a

  let filled_circle_rgba =
    foreign "filledCircleRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let filled_circle_rgba rnd ~x ~y ~rad ~r ~g ~b ~a =
    filled_circle_rgba rnd x y rad r g b a

  let ellipse_rgba =
    foreign "ellipseRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let ellipse_rgba rnd ~x ~y ~rx ~ry ~r ~g ~b ~a =
    ellipse_rgba rnd x y rx ry r g b a

  let aaellipse_rgba =
    foreign "aaellipseRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let aaellipse_rgba rnd ~x ~y ~rx ~ry ~r ~g ~b ~a =
    aaellipse_rgba rnd x y rx ry r g b a

  let filled_ellipse_rgba =
    foreign "filledEllipseRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let filled_ellipse_rgba rnd ~x ~y ~rx ~ry ~r ~g ~b ~a =
    filled_ellipse_rgba rnd x y rx ry r g b a

  let arc_rgba =
    foreign "arcRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let arc_rgba rnd ~x ~y ~rad ~start ~end_ ~r ~g ~b ~a =
    arc_rgba rnd x y rad start end_ r g b a

  let pie_rgba =
    foreign "pieRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let pie_rgba rnd ~x ~y ~rad ~start ~end_ ~r ~g ~b ~a =
    pie_rgba rnd x y rad start end_ r g b a

  let filled_pie_rgba =
    foreign "filledPieRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> returning zero_to_ok)

  let filled_pie_rgba rnd ~x ~y ~rad ~start ~end_ ~r ~g ~b ~a =
    filled_pie_rgba rnd x y rad start end_ r g b a

  let trigon_rgba =
    foreign "trigonRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> int @-> returning zero_to_ok)

  let trigon_rgba rnd ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 ~r ~g ~b ~a =
    trigon_rgba rnd x1 y1 x2 y2 x3 y3 r g b a

  let aatrigon_rgba =
    foreign "aatrigonRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> int @-> returning zero_to_ok)

  let aatrigon_rgba rnd ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 ~r ~g ~b ~a =
    aatrigon_rgba rnd x1 y1 x2 y2 x3 y3 r g b a

  let filled_trigon_rgba =
    foreign "filledTrigonRGBA"
      (renderer @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int
     @-> int @-> int @-> returning zero_to_ok)

  let filled_trigon_rgba rnd ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 ~r ~g ~b ~a =
    filled_trigon_rgba rnd x1 y1 x2 y2 x3 y3 r g b a

  let polygon_rgba =
    foreign "polygonRGBA"
      (renderer @-> ptr void @-> ptr void @-> int @-> uint8_t @-> uint8_t
     @-> uint8_t @-> uint8_t @-> returning zero_to_ok)

  let polygon_rgba rnd ~ps ~r ~g ~b ~a =
    let vx = List.map fst ps in
    let vy = List.map snd ps in
    let cax = CArray.of_list int16_t vx in
    let cay = CArray.of_list int16_t vy in
    let n = CArray.length cax in
    let px = to_voidp (CArray.start cax) in
    let py = to_voidp (CArray.start cay) in
    let r = Unsigned.UInt8.of_int r in
    let g = Unsigned.UInt8.of_int g in
    let b = Unsigned.UInt8.of_int b in
    let a = Unsigned.UInt8.of_int a in
    polygon_rgba rnd px py n r g b a

  let aapolygon_rgba =
    foreign "aapolygonRGBA"
      (renderer @-> ptr void @-> ptr void @-> int @-> uint8_t @-> uint8_t
     @-> uint8_t @-> uint8_t @-> returning zero_to_ok)

  let aapolygon_rgba rnd ~ps ~r ~g ~b ~a =
    let vx = List.map fst ps in
    let vy = List.map snd ps in
    let cax = CArray.of_list int16_t vx in
    let cay = CArray.of_list int16_t vy in
    let n = CArray.length cax in
    let px = to_voidp (CArray.start cax) in
    let py = to_voidp (CArray.start cay) in
    let r = Unsigned.UInt8.of_int r in
    let g = Unsigned.UInt8.of_int g in
    let b = Unsigned.UInt8.of_int b in
    let a = Unsigned.UInt8.of_int a in
    aapolygon_rgba rnd px py n r g b a

  let filled_polygon_rgba =
    foreign "filledPolygonRGBA"
      (renderer @-> ptr void @-> ptr void @-> int @-> uint8_t @-> uint8_t
     @-> uint8_t @-> uint8_t @-> returning zero_to_ok)

  let filled_polygon_rgba rnd ~ps ~r ~g ~b ~a =
    let vx = List.map fst ps in
    let vy = List.map snd ps in
    let cax = CArray.of_list int16_t vx in
    let cay = CArray.of_list int16_t vy in
    let n = CArray.length cax in
    let px = to_voidp (CArray.start cax) in
    let py = to_voidp (CArray.start cay) in
    let r = Unsigned.UInt8.of_int r in
    let g = Unsigned.UInt8.of_int g in
    let b = Unsigned.UInt8.of_int b in
    let a = Unsigned.UInt8.of_int a in
    filled_polygon_rgba rnd px py n r g b a

  let bezier_rgba =
    foreign "bezierRGBA"
      (renderer @-> ptr void @-> ptr void @-> int @-> int @-> uint8_t
     @-> uint8_t @-> uint8_t @-> uint8_t @-> returning zero_to_ok)

  let bezier_rgba rnd ~ps ~s ~r ~g ~b ~a =
    let vx = List.map fst ps in
    let vy = List.map snd ps in
    let cax = CArray.of_list int16_t vx in
    let cay = CArray.of_list int16_t vy in
    let n = CArray.length cax in
    let px = to_voidp (CArray.start cax) in
    let py = to_voidp (CArray.start cay) in
    let r = Unsigned.UInt8.of_int r in
    let g = Unsigned.UInt8.of_int g in
    let b = Unsigned.UInt8.of_int b in
    let a = Unsigned.UInt8.of_int a in
    bezier_rgba rnd px py n s r g b a

  let character_rgba =
    foreign "characterRGBA"
      (renderer @-> int @-> int @-> char @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let character_rgba rnd ~x ~y ~c ~r ~g ~b ~a = character_rgba rnd x y c r g b a

  let string_rgba =
    foreign "stringRGBA"
      (renderer @-> int @-> int @-> string @-> int @-> int @-> int @-> int
     @-> returning zero_to_ok)

  let string_rgba rnd ~x ~y ~s ~r ~g ~b ~a = string_rgba rnd x y s r g b a

  let set_font_rotation =
    foreign "gfxPrimitivesSetFontRotation" (int @-> returning void)

  let set_font_rotation ~rot = set_font_rotation rot
end
