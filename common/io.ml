open Gg

type view = { scale : float; translate : float * float; rotate : float }

let default = { scale = 1.0; translate = (0.0, 0.0); rotate = 0.0 }
let ( ++ ) (x, y) (dx, dy) = (x +. dx, y +. dy)
let ( *^ ) f (x, y) = (f *. x, f *. y)

let translated (x, y) t =
  let c, s = (t.scale *. cos t.rotate, t.scale *. sin t.rotate) in
  let dxy = ((c *. x) -. (s *. y), (s *. x) +. (c *. y)) in
  { t with translate = t.translate ++ dxy }

let scaled factor t = { t with scale = factor *. t.scale }
let rotated angle t = { t with rotate = angle +. t.rotate }

type t = { view : view; event : Event.t }

let translated dxy io = { io with view = translated dxy io.view }
let scaled factor io = { io with view = scaled factor io.view }
let rotated angle io = { io with view = rotated angle io.view }

type 'a scene = io:t -> 'a

let ( & ) f g ~io =
  f ~io;
  g ~io

let translate dxy fn ~io = fn ~io:(translated dxy io)
let scale factor fn ~io = fn ~io:(scaled factor io)
let rotate angle fn ~io = fn ~io:(rotated angle io)

let project ~io p =
  let x = P2.x p and y = P2.y p in
  let { scale; translate = dx, dy; rotate = angle } = io.view in
  let c, s = (scale *. cos angle, scale *. sin angle) in
  let x, y = ((c *. x) -. (s *. y), (s *. x) +. (c *. y)) in
  (x +. dx, y +. dy)

type key = Event.key

let mouse_pos ~io = Event.mouse_pos io.event
let is_pressed ~io k = Event.is_pressed io.event k
let is_up ~io k = Event.is_up io.event k
let is_down ~io k = Event.is_down io.event k
