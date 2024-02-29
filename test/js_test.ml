open Gamelle

let myfont = Font.load @@ Option.get @@ Assets.read "ubuntu-mono.ttf"
let img = Bitmap.load @@ Option.get @@ Assets.read "camel.png"
let vx = ref 0.0
let vy = ref 0.0
let x = ref 0.0
let y = ref 0.0
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

let () =
  run (fun e ->
      if Event.is_pressed e Arrow_down then vy := !vy +. 100.0;
      if Event.is_pressed e Arrow_up then vy := !vy -. 100.0;
      if Event.is_pressed e Arrow_right then vx := !vx +. 100.0;
      if Event.is_pressed e Arrow_left then vx := !vx -. 100.0;

      if Event.is_pressed e Click_left then (
        let mx, my = Event.mouse_pos e in
        let dx, dy = norm_max 100.0 (mx -. !x, my -. !y) in
        vx := !vx +. dx;
        vy := !vy +. dy);

      let speed = sqrt ((!vx *. !vx) +. (!vy *. !vy)) in
      if speed > max_speed then (
        let d = speed /. max_speed in
        vx := !vx /. d;
        vy := !vy /. d);

      x := !x +. (!vx *. dt ());
      y := !y +. (!vy *. dt ());

      vx := !vx *. 0.9;
      vy := !vy *. 0.9;

      set_color 0x000050FF;
      fill_rect (0., 0.) (window_size ());

      draw img !x !y;

      set_color 0xFF_FF_FF_FF;
      if Event.is_pressed e Click_left then set_color 0xFF_00_FF_FF;
      if Event.is_pressed e Click_right then set_color 0x00_F0_FF_FF;
      draw_circle (Event.mouse_pos e) 10.0;

      draw_string myfont ~size:48 "Hello world" 10.0 100.0)
