open Geometry

type t = { scale : float; translate : Vec.t; rotate : float }

let default = { scale = 1.0; translate = Vec.zero; rotate = 0.0 }
let ( *^ ) f (x, y) = (f *. x, f *. y)

let translate dxy t =
  let x, y = Vec.to_tuple dxy in
  let c, s = (t.scale *. cos t.rotate, t.scale *. sin t.rotate) in
  let dxy = Vec.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  { t with translate = Vec.(t.translate + dxy) }

let scale factor t = { t with scale = factor *. t.scale }
let rotate angle t = { t with rotate = angle +. t.rotate }

let project { scale; translate; rotate } p =
  let x, y = Vec.to_tuple p in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  let p = Point.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  Vec.(p + translate)
