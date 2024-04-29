open Gamelle

let myfont = Font.default
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
let black = Color.black
let red = Color.red
let green = Color.green
let blue = Color.blue
let yellow = Color.v 1.0 1.0 0.0 1.0

let update ~io { x; y; vx; vy; _ } =
  if Event.is_pressed ~io `escape then raise Exit;
  let y =
    if Event.is_pressed ~io `wheel then y +. (10.0 *. Event.wheel_delta ~io)
    else y
  in
  let vy = if Event.is_pressed ~io `arrow_down then vy +. 100.0 else vy in
  let vy = if Event.is_pressed ~io `arrow_up then vy -. 100.0 else vy in
  let vx = if Event.is_pressed ~io `arrow_right then vx +. 100.0 else vx in
  let vx = if Event.is_pressed ~io `arrow_left then vx -. 100.0 else vx in
  let mx, my = Vec.to_tuple @@ Event.mouse_pos ~io in

  if Event.is_down ~io `click_left then (
    cursor := not !cursor;
    show_cursor ~io !cursor;
    Sound.play ~io Assets.stick);

  let vx, vy =
    if Event.is_pressed ~io `click_left then
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

  let dt = dt ~io in
  let x = x +. (vx *. dt) in
  let y = y +. (vy *. dt) in

  let vx = vx *. 0.9 in
  let vy = vy *. 0.9 in

  Window.set_size ~io (Size.v 800. 800.);
  Box.fill ~io ~color:black (Window.box ~io);
  Text.draw ~io ~color:Color.white ~size:30 "Hello World!" ~at:Vec.zero;
  Text.draw ~io:(View.scaled 2.0 io) ~color:Color.white ~size:30 "Hello World!"
    ~at:Vec.zero;
  View.(
    translate (Vec.v mx my)
      (Circle.fill ~color:red (Circle.v (Point.v 0.0 0.0) 10.0))
    & translate (Vec.v x y)
        (scale 3.0
           (Box.draw ~color:yellow (Box.v (Point.v 0. 0.) (Size.v 100.0 100.0))
           & translate (Vec.v (75. /. 2.) (59. /. 2.))
             @@ rotate (1.0 *. clock ~io)
             @@ translate (Vec.v (-75. /. 2.) (-59. /. 2.))
             @@ (Box.fill ~color:blue (Box.v (Point.v 0. 0.) (Size.v 75. 59.))
                & Box.draw ~color:red (Box.v (Point.v 0. 0.) (Size.v 75. 59.))
                & draw img (Point.v 0. 0.)
                & Segment.draw ~color:red
                    (Segment.v (Point.v 0. 0.) (Point.v 75. 59.))
                & translate (Vec.v 5. 5.)
                    (Polygon.fill ~color:yellow
                       (Polygon.v
                          [ Point.v 20. 0.; Point.v 30. 30.; Point.v 15. 40. ]))
                & Polygon.draw ~color:green
                    (Polygon.v
                       [ Point.v 20. 0.; Point.v 30. 30.; Point.v 15. 40. ])
                & Circle.draw ~color:green
                    (Circle.v (Point.v (75.0 /. 2.) (59.0 /. 2.)) 10.)))))
    ~io;
  { x; y; vx; vy; mx; my }

let () = run { mx = 0.0; my = 0.0; x = 0.0; y = 0.0; vx = 0.0; vy = 0.0 } update
