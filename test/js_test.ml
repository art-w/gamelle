open Gamelle

let myfont = Assets.ubuntu_mono
let img = Assets.camel
let max_speed = 1000.0

let norm (x, y) =
  let d = sqrt ((x *. x) +. (y *. y)) in
  if d > 0.0 then (x /. d, y /. d) else (x, y)

let norm_max max_speed (x, y) =
  let d = sqrt ((x *. x) +. (y *. y)) in
  if d > max_speed then
    let d = d /. max_speed in
    (x /. d, y /. d)
  else (x, y)

type state = { x : float; y : float; vx : float; vy : float }

let update e { x; y; vx; vy } =
  let vy = if Event.is_pressed e Arrow_down then vy +. 100.0 else vy in
  let vy = if Event.is_pressed e Arrow_up then vy -. 100.0 else vy in
  let vx = if Event.is_pressed e Arrow_right then vx +. 100.0 else vx in
  let vx = if Event.is_pressed e Arrow_left then vx -. 100.0 else vx in

  let vx, vy =
    if Event.is_pressed e Click_left then (
      Sound.play Assets.stick;
      let mx, my = Event.mouse_pos e in
      let dx, dy = norm_max 100.0 (mx -. x, my -. y) in
      (vx +. dx, vy +. dy))
    else (vx, vy)
  in

  let speed = sqrt ((vx *. vx) +. (vy *. vy)) in
  let vx, vy =
    if speed > max_speed then
      let d = speed /. max_speed in
      (vx /. d, vy /. d)
    else (vx, vy)
  in

  let x = x +. (vx *. dt ()) in
  let y = y +. (vy *. dt ()) in

  let vx = vx *. 0.9 in
  let vy = vy *. 0.9 in
  { x; y; vx; vy }

let render { x; y; _ } =
  set_color 0x000050FF;
  fill_rect (0., 0.) (window_size ());
  draw img x y;
  draw_string myfont ~size:48 "Hello world" 10.0 100.0

let () = run { x = 0.0; y = 0.0; vx = 0.0; vy = 0.0 } ~update ~render
