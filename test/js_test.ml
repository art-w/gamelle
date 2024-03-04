open Gamelle
open Gg

let myfont = Assets.ubuntu_mono
let img = Assets.camel
let max_speed = 1000.045

let norm (x, y) =
  let d = sqrt ((x *. x) +. (y *. y)) in
  if d > 0.0 then (x /. d, y /. d) else (x, y)

let norm_max max_speed (x, y) =
  let d = sqrt ((x *. x) +. (y *. y)) in
  if d > max_speed then
    let d = d /. max_speed in
    (x /. d, y /. d)
  else (x, y)

type state = {
  x : float;
  y : float;
  vx : float;
  vy : float;
  mx : float;
  my : float;
}

let cursor = ref true

let update e { x; y; vx; vy; _ } =
  if Event.is_pressed e Escape then raise Exit;
  let y = if Event.is_pressed e Wheel_down then y +. 10.0 else y in
  let y = if Event.is_pressed e Wheel_up then y -. 10.0 else y in
  let vy = if Event.is_pressed e Arrow_down then vy +. 100.0 else vy in
  let vy = if Event.is_pressed e Arrow_up then vy -. 100.0 else vy in
  let vx = if Event.is_pressed e Arrow_right then vx +. 100.0 else vx in
  let vx = if Event.is_pressed e Arrow_left then vx -. 100.0 else vx in
  let mx, my = Event.mouse_pos e in

  if Event.is_down e Click_left then (
    cursor := not !cursor;
    show_cursor !cursor;
    Sound.play Assets.stick);

  let vx, vy =
    if Event.is_pressed e Click_left then
      let dx, dy = norm_max 100.0 (mx -. x, my -. y) in
      (vx +. dx, vy +. dy)
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
  { x; y; vx; vy; mx; my }

let black = Color.black
let red = Color.red
let green = Color.green
let blue = Color.blue
let yellow = Color.v 1.0 1.0 0.0 1.0

let render ~view { x; y; mx; my; _ } =
  Window.set_size (800, 800);
  fill_rect ~color:black ~view (Window.box ());
  View.(
    translate (mx, my) (fill_circle ~color:red (P2.v 0.0 0.0) 10.0)
    & translate (x, y)
        (scale 3.0
           (draw_rect ~color:yellow (Box2.v (P2.v 0. 0.) (Size2.v 100.0 100.0))
           & translate (75. /. 2., 59. /. 2.)
             @@ rotate (1.0 *. clock ())
             @@ translate (-75. /. 2., -59. /. 2.)
             @@ (fill_rect ~color:blue (Box2.v (P2.v 0. 0.) (Size2.v 75. 59.))
                & draw_rect ~color:red (Box2.v (P2.v 0. 0.) (Size2.v 75. 59.))
                & draw img (P2.v 0. 0.)
                & draw_line ~color:red (P2.v 0. 0.) (P2.v 75. 59.)
                & translate (5.0, 5.0)
                    (fill_poly ~color:yellow
                       [ P2.v 20. 0.; P2.v 30. 30.; P2.v 15. 40. ])
                & draw_poly ~color:green
                    [ P2.v 20. 0.; P2.v 30. 30.; P2.v 15. 40. ]
                & draw_circle ~color:green (P2.v (75.0 /. 2.) (59.0 /. 2.)) 10.
                ))))
    ~view

let () =
  run
    { mx = 0.0; my = 0.0; x = 0.0; y = 0.0; vx = 0.0; vy = 0.0 }
    ~update ~render
