open Gamelle

type player = { shape : Physics.t; jumps : int; grounded : bool }

type state = {
  player1 : player;
  player2 : player;
  ball : Physics.t;
  points1 : int;
  points2 : int;
}

let restitution = 1.0
let player_radius = 80.0
let bottom = 500.0
let horz_speed = 2000.0

let init_ball () =
  Physics.add_velocity (Vec.v (Random.float 30.0 -. 15.0) (-1_000.0))
  @@ Physics.v ~mass:1.0 ~restitution:1.0 ~kind:Movable
       (Shape.circle (Circle.v (Point.v 500.0 0.0) 50.0))

let init_player pos =
  {
    shape =
      Physics.v ~restitution:0.8 ~kind:Movable ~mass:1000.0
        (Shape.circle (Circle.v pos player_radius));
    jumps = 0;
    grounded = false;
  }

let initial_state =
  {
    player1 = init_player (Point.v 200.0 200.0);
    player2 = init_player (Point.v 800.0 200.0);
    ball = init_ball ();
    points1 = 0;
    points2 = 0;
  }

let world =
  [
    Physics.v ~restitution ~kind:Immovable
      (Shape.circle (Circle.v (Point.v 500.0 200.0) 10.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 0.0 (-1500.0)) (Vec.v 1000.0 1010.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 490.0 200.0) (Vec.v 20.0 1000.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v (-1000.0) (-1000.0)) (Vec.v 1010.0 1500.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 1000.0 (-1000.0)) (Vec.v 1010.0 1500.0));
    Physics.v ~restitution:0.0 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v (-1000.0) bottom) (Vec.v 3500.0 1000.0));
  ]

let block_player1 =
  Physics.v ~restitution ~kind:Immovable
  @@ Shape.rect
  @@ Box.v (Point.v 500.0 (-1000.0)) (Vec.v 1000.0 2000.0)

let block_player2 =
  Physics.v ~restitution ~kind:Immovable
  @@ Shape.rect
  @@ Box.v (Point.v (-500.0) (-1000.0)) (Vec.v 1000.0 2000.0)

let update_player ~io ~player:{ shape = player; jumps; grounded } ~gravity left
    right up down =
  let dt = dt ~io in
  let grounded, player =
    let touching_ground =
      Vec.y (Physics.center player) >= bottom -. player_radius -. 10.0
    in
    if touching_ground && not grounded then
      (true, Physics.(set_rot_velocity 0.0 @@ set_velocity Vec.zero player))
    else (touching_ground, player)
  in
  let player =
    if Input.is_pressed ~io left then
      Physics.add_velocity (Vec.v (-.horz_speed *. dt) 0.0) player
    else player
  in
  let player =
    if Input.is_pressed ~io right then
      Physics.add_velocity (Vec.v (horz_speed *. dt) 0.0) player
    else player
  in
  let player =
    if Input.is_pressed ~io down then
      Physics.add_velocity (Vec.v 0.0 (10_000.0 *. dt)) player
    else player
  in
  let jumps = if grounded then 0 else jumps in
  let player, jumps =
    if Input.is_down ~io up && jumps < 2 then
      (Physics.add_velocity (Vec.v 0.0 (-60000.0 *. dt)) player, jumps + 1)
    else (player, jumps)
  in
  let player = Physics.add_velocity gravity player in
  let player = Physics.update ~dt player in
  { shape = player; jumps; grounded }

let rec loop ~io ({ player1; player2; ball; _ } as state) =
  let io = View.translate (Vec.v 0.0 500.0) io in
  Window.set_size ~io (Size.v 1010. 1020.);
  Box.fill ~io ~color:Color.black (Window.box ~io);
  let state =
    if Input.is_pressed ~io `escape then raise Exit
    else if Input.is_down ~io (`input_char "r") then initial_state
    else if Vec.y (Physics.center ball) > 440.0 then
      if Vec.x (Physics.center ball) < 500.0 then
        { state with ball = init_ball (); points2 = state.points2 + 1 }
      else { state with ball = init_ball (); points1 = state.points1 + 1 }
    else
      let dt = dt ~io in
      let gravity = Vec.v 0.0 (1500.0 *. dt) in
      let ball = Physics.add_velocity gravity ball in
      let ball = Physics.update ~dt ball in
      let player2 =
        update_player ~io ~gravity `arrow_left `arrow_right `arrow_up
          `arrow_down ~player:player2
      in
      let player1 =
        update_player ~io ~gravity (`physical_char 'a') (`physical_char 'd')
          (`physical_char 'w') (`physical_char 's') ~player:player1
      in
      let[@warning "-partial-match"] (player1_shape :: player2_shape :: ball
                                    :: _) =
        Physics.fix_collisions (player1.shape :: player2.shape :: ball :: world)
      in
      let[@warning "-partial-match"] (player1_shape :: _) =
        Physics.fix_collisions [ player1_shape; block_player1 ]
      in
      let[@warning "-partial-match"] (player2_shape :: _) =
        Physics.fix_collisions [ player2_shape; block_player2 ]
      in
      let player1 = { player1 with shape = player1_shape } in
      let player2 = { player2 with shape = player2_shape } in
      List.iter (Physics.fill ~io ~color:Color.white) world;
      Physics.fill ~io ~color:Color.blue player1.shape;
      Physics.fill ~io ~color:Color.blue player2.shape;
      Physics.fill ~io ~color:Color.red ball;
      List.iter (Physics.draw ~io) world;
      Physics.draw ~io player1.shape;
      Physics.draw ~io player2.shape;
      Physics.draw ~io ball;
      Text.draw ~io ~size:40 ~color:Color.white
        (string_of_int state.points1)
        ~at:(Point.v 20.0 10.0);
      Text.draw ~io ~size:40 ~color:Color.white
        (string_of_int state.points2)
        ~at:(Point.v 960.0 10.0);
      { state with player1; player2; ball }
  in
  next_frame ~io;
  loop ~io state

let () = Gamelle.run (loop initial_state)
