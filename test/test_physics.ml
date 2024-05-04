open Gamelle

let world =
  [
    Physics.make ~kind:Physics.Immovable
      (Shape.rotate ~angle:0.2 @@ Shape.rect
      @@ Box.v (Vec.v 0. 300.) (Vec.v 300. 10.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rotate ~angle:(-0.2) @@ Shape.rect
      @@ Box.v (Vec.v 210. 500.) (Vec.v 300. 10.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box.v (Vec.v 0. 0.) (Vec.v 10. 800.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box.v (Vec.v 500. 0.) (Vec.v 10. 800.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box.v (Vec.v 0. 800.) (Vec.v 520. 10.));
  ]

let random_size1 () = 5.0 +. Random.float 30.0
let random_size2 () = Size.v (2.0 *. random_size1 ()) (2.0 *. random_size1 ())

let () =
  run world @@ fun ~io world ->
  Window.show_cursor ~io true;
  Window.set_size ~io (Size.v 510.0 810.0);
  if Input.is_up ~io `escape then raise Exit;
  let world =
    if Input.is_down ~io `click_left then
      let obj =
        Physics.make
        @@ Shape.circle (Circle.v (Input.mouse_pos ~io) (random_size1 ()))
      in
      obj :: world
    else if Input.is_down ~io `click_right then
      let obj =
        Physics.make @@ Shape.rect
        @@ Box.v (Input.mouse_pos ~io) (random_size2 ())
      in
      obj :: world
    else world
  in
  let dt = dt ~io in
  let gravity = Vec.v 0.0 (1500.0 *. dt) in
  let world = List.map (Physics.add_velocity gravity) world in
  let world = List.map (Physics.update ~dt) world in
  let world = Physics.fix_collisions world in

  let color = Color.v 1.0 1.0 0.0 0.2 in
  List.iter (Physics.fill ~io ~color) world;
  let color = Color.v 1.0 1.0 0.0 1.0 in
  List.iter (Physics.draw ~io ~color) world;
  world
