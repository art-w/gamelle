open Gg
open Gamelle

let world =
  [
    Physics.make ~kind:Physics.Immovable
      (Shape.rotate ~angle:0.2 @@ Shape.rect
      @@ Box2.v (V2.v 0. 300.) (V2.v 300. 10.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rotate ~angle:(-0.2) @@ Shape.rect
      @@ Box2.v (V2.v 210. 500.) (V2.v 300. 10.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box2.v (V2.v 0. 0.) (V2.v 10. 800.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box2.v (V2.v 500. 0.) (V2.v 10. 800.));
    Physics.make ~kind:Physics.Immovable
      (Shape.rect @@ Box2.v (V2.v 0. 800.) (V2.v 520. 10.));
  ]

let random_size1 () = 5.0 +. Random.float 30.0
let random_size2 () = Size2.v (2.0 *. random_size1 ()) (2.0 *. random_size1 ())

let () =
  run world @@ fun ~io world ->
  show_cursor true;
  if Event.is_up ~io `escape then raise Exit;
  let world =
    if Event.is_down ~io `click_left then
      let obj =
        Physics.make
        @@ Shape.circle (V2.of_tuple @@ Event.mouse_pos ~io) (random_size1 ())
      in
      obj :: world
    else if Event.is_down ~io `click_right then
      let obj =
        Physics.make @@ Shape.rect
        @@ Box2.v (V2.of_tuple @@ Event.mouse_pos ~io) (random_size2 ())
      in
      obj :: world
    else world
  in
  let dt = dt () in
  let gravity = V2.v 0.0 (1500.0 *. dt) in
  let world = List.map (Physics.add_velocity gravity) world in
  let world = List.map (Physics.update ~dt) world in
  let world = Physics.fix_collisions world in
  List.iter (Physics.draw ~io) world;
  world
