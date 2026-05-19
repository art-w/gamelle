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
  pts
  |> List.iteri begin fun i p ->
      let x, y = project ~io p in
      Raylib.CArray.set arr i (v2 x y)
    end;
  (arr, n)

let draw_poly ~io ?color poly =
  let pts = Polygon.points poly in
  let n = List.length pts in
  if n >= 2 then begin
    let color = get_color ~io color in
    let arr = Raylib.CArray.make Raylib.Vector2.t (n + 1) in
    pts
    |> List.iteri begin fun i p ->
        let x, y = project ~io p in
        Raylib.CArray.set arr i (v2 x y)
      end;
    let x, y = project ~io (List.hd pts) in
    Raylib.CArray.set arr n (v2 x y);
    with_scissor ~io @@ fun () ->
    Raylib.draw_line_strip (Raylib.CArray.start arr) (n + 1) color
  end

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
    begin
      Polygon.v
        [
          Box.top_left rect;
          Box.top_right rect;
          Box.bottom_right rect;
          Box.bottom_left rect;
        ]
    end

let fill_rect ~io ?color rect =
  let x0, y0 = project ~io (Box.top_left rect) in
  let x1, y1 = project ~io (Box.bottom_right rect) in
  with_scissor ~io @@ fun () ->
  Raylib.draw_rectangle (int_of_float x0) (int_of_float y0)
    (int_of_float (x1 -. x0))
    (int_of_float (y1 -. y0))
    (get_color ~io color)

(* --- SDF circle shaders --- *)

let vs =
  {glsl|
#version 330
in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;
uniform mat4 mvp;
out vec2 fragTexCoord;
out vec4 fragColor;
void main() {
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
|glsl}

let fs_fill =
  {glsl|
#version 330
precision mediump float;
uniform vec2 center;
uniform float radius;
uniform vec4 circleColor;
uniform float screenHeight;
out vec4 finalColor;
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float d = length(pos - center) - radius;
    float alpha = 1.0 - smoothstep(-1.0, 1.0, d);
    finalColor = vec4(circleColor.rgb, circleColor.a * alpha);
}
|glsl}

let fs_draw =
  {glsl|
#version 330
precision mediump float;
uniform vec2 center;
uniform float radius;
uniform vec4 circleColor;
uniform float screenHeight;
out vec4 finalColor;
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float d = abs(length(pos - center) - radius) - 0.5;
    float alpha = 1.0 - smoothstep(-1.0, 1.0, d);
    finalColor = vec4(circleColor.rgb, circleColor.a * alpha);
}
|glsl}

type circle_shader = {
  shader : Raylib.Shader.t;
  loc_center : Raylib.ShaderLoc.t;
  loc_radius : Raylib.ShaderLoc.t;
  loc_color : Raylib.ShaderLoc.t;
  loc_screen_height : Raylib.ShaderLoc.t;
}

let load_circle_shader fs =
  let shader = Raylib.load_shader_from_memory vs fs in
  {
    shader;
    loc_center = Raylib.get_shader_location shader "center";
    loc_radius = Raylib.get_shader_location shader "radius";
    loc_color = Raylib.get_shader_location shader "circleColor";
    loc_screen_height = Raylib.get_shader_location shader "screenHeight";
  }

let fill_shader : circle_shader option ref = ref None
let draw_shader : circle_shader option ref = ref None

let get_shader r fs =
  match !r with
  | Some s -> s
  | None ->
      let s = load_circle_shader fs in
      r := Some s;
      s

let buf1 = Ctypes.CArray.make Ctypes.float 1
let buf2 = Ctypes.CArray.make Ctypes.float 2
let buf4 = Ctypes.CArray.make Ctypes.float 4

let set_float shader loc v =
  Ctypes.CArray.set buf1 0 v;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf1 |> to_voidp)
    Raylib.ShaderUniformDataType.Float

let set_vec2 shader loc x y =
  Ctypes.CArray.set buf2 0 x;
  Ctypes.CArray.set buf2 1 y;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf2 |> to_voidp)
    Raylib.ShaderUniformDataType.Vec2

let set_vec4 shader loc x y z w =
  Ctypes.CArray.set buf4 0 x;
  Ctypes.CArray.set buf4 1 y;
  Ctypes.CArray.set buf4 2 z;
  Ctypes.CArray.set buf4 3 w;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf4 |> to_voidp)
    Raylib.ShaderUniformDataType.Vec4

let circle_uniforms s cx cy radius color =
  set_vec2 s.shader s.loc_center cx cy;
  set_float s.shader s.loc_radius radius;
  let cr = float (Raylib.Color.r color) /. 255.0 in
  let cg = float (Raylib.Color.g color) /. 255.0 in
  let cb = float (Raylib.Color.b color) /. 255.0 in
  let ca = float (Raylib.Color.a color) /. 255.0 in
  set_vec4 s.shader s.loc_color cr cg cb ca;
  set_float s.shader s.loc_screen_height (float (Raylib.get_render_height ()));
  let pad = 2.0 in
  let qx = int_of_float (cx -. radius -. pad) in
  let qy = int_of_float (cy -. radius -. pad) in
  let qs = int_of_float (2.0 *. (radius +. pad)) in
  (qx, qy, qs)

let draw_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let cx, cy = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  let s = get_shader draw_shader fs_draw in
  let qx, qy, qs = circle_uniforms s cx cy radius color in
  with_scissor ~io @@ fun () ->
  Raylib.begin_shader_mode s.shader;
  Raylib.draw_rectangle qx qy qs qs Raylib.Color.white;
  Raylib.end_shader_mode ()

let fill_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let cx, cy = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  let s = get_shader fill_shader fs_fill in
  let qx, qy, qs = circle_uniforms s cx cy radius color in
  with_scissor ~io @@ fun () ->
  Raylib.begin_shader_mode s.shader;
  Raylib.draw_rectangle qx qy qs qs Raylib.Color.white;
  Raylib.end_shader_mode ()
