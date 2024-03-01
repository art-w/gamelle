type t = { scale : float; translate : float * float; rotate : float }

let default = { scale = 1.0; translate = (0.0, 0.0); rotate = 0.0 }
let ( ++ ) (x, y) (dx, dy) = (x +. dx, y +. dy)
let ( *^ ) f (x, y) = (f *. x, f *. y)

let translated (x, y) t =
  let c, s = (t.scale *. cos t.rotate, t.scale *. sin t.rotate) in
  let dxy = ((c *. x) -. (s *. y), (s *. x) +. (c *. y)) in
  { t with translate = t.translate ++ dxy }

let scaled factor t = { t with scale = factor *. t.scale }
let rotated angle t = { t with rotate = angle +. t.rotate }

type 'a scene = view:t -> 'a

let ( & ) f g ~view =
  f ~view;
  g ~view

let translate dxy fn ~view = fn ~view:(translated dxy view)
let scale factor fn ~view = fn ~view:(scaled factor view)
let rotate angle fn ~view = fn ~view:(rotated angle view)
