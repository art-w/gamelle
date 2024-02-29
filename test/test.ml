let bmp = Gamelle.Bitmap.(scale 2.0 Assets.camel)
let font = Assets.ubuntu_mono
let boing = Gamelle.Sound.load "/usr/lib/slack/resources/animal_stick.mp3"

(* let count = ref 0.0 *)

let int = int_of_float
let x, y = (ref 100.0, ref 100.0)

let update ev () =
  if Gamelle.Event.is_pressed ev (Char 'b') then Gamelle.Sound.play boing;

  let dt = Gamelle.dt () in

  if Gamelle.Event.is_pressed ev Arrow_left then x := !x -. (100.0 *. dt);
  if Gamelle.Event.is_pressed ev Arrow_right then x := !x +. (100.0 *. dt);
  if Gamelle.Event.is_pressed ev Arrow_down then y := !y +. (100.0 *. dt);
  if Gamelle.Event.is_pressed ev Arrow_up then y := !y -. (100.0 *. dt);
  ()

let render () =
  (* count := !count +. 1.0 ; *)
  Format.printf "frames per seconds: %f@." (1.0 /. Gamelle.dt ());
  let count = 10. *. Gamelle.clock () in
  Gamelle.set_color 0x000000FF;
  Gamelle.fill_rect (0.0, 0.0) (1000.0, 1000.0);

  Gamelle.set_color 0xF00F00FF;
  Gamelle.fill_rect (100.0, 100.0) (100.0, 100.0);
  Gamelle.set_color 0xFFFF00FF;
  Gamelle.draw_rect (100.0, 100.0) (100.0, 100.0);
  Gamelle.set_color 0xFFFFFFFF;
  Gamelle.draw_line (100.0, 100.0) (200.0, 200.0);
  Gamelle.draw_thick_line ~stroke:10.0 (200.0, 100.0) (100.0, 200.0);

  Gamelle.draw (Gamelle.Bitmap.rotate (5. *. count) bmp) !x !y;

  let size = int (30.0 +. (20.0 *. sin (count /. 10.0))) in

  Gamelle.set_color 0xFF0000FF;
  Gamelle.draw_rect
    ( 100.0 +. (100.0 *. cos (count /. 10.0)),
      10.0 +. (5.0 *. sin (count /. 10.0)) )
    (10.0, 10.0);

  Gamelle.set_color 0xFFFFFFFF;
  Gamelle.draw_string font ~size "Hi!!"
    (100.0 +. (100.0 *. cos (count /. 10.0)))
    (10.0 +. (5.0 *. sin (count /. 10.0)));

  Gamelle.draw_poly [ (0.0, 0.0); (100.0, 50.0); (150.0, 100.0); (50.0, 250.0) ];

  Gamelle.draw_circle (200.0, 200.0) 10.0;
  Gamelle.draw_circle (200.0, 200.0) 20.0;
  Gamelle.draw_circle (200.0, 200.0) 30.0;
  Gamelle.draw_circle (200.0, 200.0) 40.0;

  Gamelle.set_color 0xFF000050;
  Gamelle.fill_circle (300.0, 200.0) 40.0;
  Gamelle.fill_circle (300.0, 200.0) 30.0;
  Gamelle.fill_circle (300.0, 200.0) 20.0;
  Gamelle.fill_circle (300.0, 200.0) 10.0;

  Gamelle.set_color 0x00FFFFFF;
  Gamelle.fill_poly
  @@ List.map
       (fun (x, y) -> (x +. 210., y +. 210.))
       [
         (0.0, 0.0);
         (100.0, 50.0);
         (150.0, 100.0 +. Random.float 100.0);
         (50.0, 250.0);
       ];
  ()

let () = Gamelle.run () ~update ~render
