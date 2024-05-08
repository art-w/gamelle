open Geometry
open Xy

type t = { scale : float; translate : Vec.t; rotate : float }

let default = { scale = 1.0; translate = Vec.zero; rotate = 0.0 }
let ( *^ ) f (x, y) = (f *. x, f *. y)

let translate dxy t =
  let x, y = (dxy.x, dxy.y) in
  let c, s = (t.scale *. cos t.rotate, t.scale *. sin t.rotate) in
  let dxy = Vec.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  { t with translate = Vec.(t.translate + dxy) }

let scale factor t = { t with scale = factor *. t.scale }
let rotate angle t = { t with rotate = angle +. t.rotate }

let project { scale; translate = tr; rotate } p =
  let x, y = (p.x, p.y) in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  let p = Point.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  Point.(p + tr)

let inv_project { scale; translate = tr; rotate } p =
  let rotate = -.rotate in
  let scale = 1.0 /. scale in
  let p = Point.(p - tr) in
  let x, y = (p.x, p.y) in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  Point.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y))

let project_box t box =
  let top_left = project t (Box.top_left box) in
  let size = Size.(t.scale * Box.size box) in
  Box.v top_left size
