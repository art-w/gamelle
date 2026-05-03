open Gamelle

let screen_w = 800.0
let screen_h = 500.0
let ground_y = 460.0
let player_size = 40.0
let gravity = 2500.0
let player_speed = 220.0
let jump_v = -850.0

let make_ground () =
  Physics.v ~kind:Immovable ~restitution:0.0
    (Shape.rect (Box.v (Point.v (-50000.0) ground_y) (Vec.v 100000.0 200.0)))

let make_triangle cx =
  let half_w = 28.0 in
  Physics.v ~kind:Immovable ~restitution:0.0
    (Shape.polygon
       (Polygon.v
          [
            Point.v (cx -. half_w) ground_y;
            Point.v (cx +. half_w) ground_y;
            Point.v cx (ground_y -. 60.0);
          ]))

let make_player () =
  Physics.v ~kind:Movable ~restitution:0.0 ~mass:50.0
    (Shape.rect
       (Box.v
          (Point.v 80.0 (ground_y -. player_size))
          (Vec.v player_size player_size)))

let triangle_xs =
  [
    350.0;
    500.0;
    660.0;
    670.0;
    900.0;
    1050.0;
    1200.0;
    1210.0;
    1220.0;
    1500.0;
    1650.0;
    1800.0;
  ]

let level_end_x = 2100.0

type state = {
  player : Physics.t;
  ground : Physics.t;
  triangles : Physics.t list;
}

let init () =
  {
    player = make_player ();
    ground = make_ground ();
    triangles = List.map make_triangle triangle_xs;
  }

let is_on_ground player ground =
  Shape.intersect (Physics.shape player) (Physics.shape ground)

let draw_flag ~io =
  let pole_top = ground_y -. 120.0 in
  Segment.draw ~io ~color:Color.white
    (Segment.v (Point.v level_end_x pole_top) (Point.v level_end_x ground_y));
  Polygon.fill ~io ~color:Color.yellow
    (Polygon.v
       [
         Point.v level_end_x pole_top;
         Point.v (level_end_x +. 40.0) (pole_top +. 20.0);
         Point.v level_end_x (pole_top +. 40.0);
       ])

let rec victory_screen ~io =
  if Input.is_pressed ~io `escape then raise Exit;
  Box.fill ~io ~color:Color.black (Window.box ~io);
  Text.draw ~io ~color:Color.yellow ~size:40 "You win!"
    ~at:(Point.v ((screen_w /. 2.0) -. 70.0) ((screen_h /. 2.0) -. 40.0));
  Text.draw ~io ~color:Color.white ~size:25 "Press R to play again"
    ~at:(Point.v ((screen_w /. 2.0) -. 135.0) ((screen_h /. 2.0) +. 20.0));
  next_frame ~io;
  if Input.is_down ~io (`input_char "r") then () else victory_screen ~io

let rec dead_screen ~io =
  if Input.is_pressed ~io `escape then raise Exit;
  Box.fill ~io ~color:Color.black (Window.box ~io);
  Text.draw ~io ~color:Color.white ~size:30 "You died! Press R to restart"
    ~at:(Point.v ((screen_w /. 2.0) -. 200.0) ((screen_h /. 2.0) -. 15.0));
  next_frame ~io;
  if Input.is_down ~io (`input_char "r") then () else dead_screen ~io

let rec loop ~io state =
  if Input.is_pressed ~io `escape then raise Exit;
  let dt = dt ~io in
  let { player; ground; triangles } = state in
  let vy = Vec.y (Physics.velocity player) in
  let player = Physics.set_velocity (Vec.v player_speed vy) player in
  let player = Physics.add_velocity (Vec.v 0.0 (gravity *. dt)) player in
  let player =
    if is_on_ground player ground && Input.is_pressed ~io `space then
      Physics.set_velocity (Vec.v player_speed jump_v) player
    else player
  in
  let player = Physics.update ~dt player in
  let dead =
    List.exists
      (fun tri -> Shape.intersect (Physics.shape player) (Physics.shape tri))
      triangles
    || Vec.y (Physics.center player) > screen_h +. 50.0
  in
  if dead then (
    dead_screen ~io;
    loop ~io (init ()))
  else
    let all = player :: ground :: triangles in
    let data = Physics.precompute_collisions all in
    let player = Physics.fix_collisions_with_data data player in
    let player_x = Vec.x (Physics.center player) in
    let won = player_x >= level_end_x in
    let draw_io = View.translate (Vec.v (200.0 -. player_x) 0.0) io in
    Box.fill ~io:draw_io ~color:Color.black (Window.box ~io:draw_io);
    Physics.fill ~io:draw_io ~color:(Color.rgb 80 160 80) ground;
    List.iter (Physics.fill ~io:draw_io ~color:Color.red) triangles;
    draw_flag ~io:draw_io;
    Physics.fill ~io:draw_io ~color:Color.blue player;
    next_frame ~io;
    if won then (
      victory_screen ~io;
      loop ~io (init ()))
    else loop ~io { state with player }

let () =
  run_no_loop (fun ~io ->
      Window.set_size ~io (Size.v screen_w screen_h);
      loop ~io (init ()))
