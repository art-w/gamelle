open Gamelle
open Geometry

type player = {pos: float; score: int}

type state =
  {player_left: player; player_right: player; ball_speed: v2; ball_pos: p2}

type side = Left | Right

let player ~side state =
  match side with Left -> state.player_left | Right -> state.player_right

let set_player ~side state player =
  match side with
  | Left ->
      {state with player_left= player}
  | Right ->
      {state with player_right= player}

let init =
  { player_left= {pos= 120.; score= 0}
  ; player_right= {pos= 120.; score= 0}
  ; ball_speed= V2.v 1. 0.
  ; ball_pos= P2.v 200. 120. }

let ball_noise = Box2.v (P2.v (-0.01) (-0.01)) (P2.v (-0.02) (-0.02))

let box_game = Box2.v (P2.v 0. 0.) (V2.v 400. 220.)

let court = Box2.v (P2.v 0. 20.) (V2.v 400. 200.)

let player_left_x = Box2.ox court +. 20.

let player_right_x = Box2.w court -. 20.

let player_height = 50.

let player_grip = 0.4

let ball_x_boost = 0.2

let player_speed {ball_speed; _} = 2. *. V2.norm ball_speed

let player_segment_x x player =
  Segment.v
    (V2.v x (player.pos -. (player_height /. 2.)))
    (V2.v x (player.pos +. (player_height /. 2.)))

let player_left_segment state = player_segment_x player_left_x state.player_left

let player_right_segment state =
  player_segment_x player_right_x state.player_right

let player_segment ~side state =
  match side with
  | Left ->
      player_left_segment state
  | Right ->
      player_right_segment state

let update_player ~x ~player ~delta_y =
  let pos = player.pos +. delta_y in
  let new_player = {player with pos} in
  let start, end_ = Segment.to_tuple (player_segment_x x new_player) in
  if Box2.mem start court && Box2.mem end_ court then new_player else player

let reflexion ray edge =
  let normal vec = V2.(v (-1. *. y vec) (x vec)) in
  let normal = V2.unit (normal edge) in
  V2.(ray - (2. * (dot ray normal * normal)))

let incr_score player = {player with score= player.score + 1}

let new_point_ball_speed ball_speed =
  let length = V2.((norm init.ball_speed +. norm ball_speed) /. 2.) in
  V2.(-.length * unit ball_speed)

let goal ~side state =
  let ball_speed = state.ball_speed in
  let ball_speed = new_point_ball_speed ball_speed in
  let ball_pos = init.ball_pos in
  let player = incr_score (player ~side state) in
  set_player ~side {state with ball_pos; ball_speed} player

let player_collision ~side ~player_speed state =
  let player_vector = Segment.vector (player_segment ~side state) in
  let ball_speed =
    V2.(
      reflexion state.ball_speed player_vector
      + v ball_x_boost (player_grip *. player_speed)
      + Box.random_mem ball_noise )
  in
  {state with ball_speed}

let wall_collision state wall =
  let ball_speed =
    V2.(
      reflexion state.ball_speed (Segment.vector wall)
      + Box.random_mem ball_noise )
  in
  {state with ball_speed}

let tick state ~player_left_speed ~player_right_speed =
  let {ball_pos; ball_speed; _} = state in
  let new_ball_pos = V2.(ball_pos + ball_speed) in
  let ball_pos = state.ball_pos in
  let top, right, bottom, left = Box.sides court in
  let ball_segment = Segment.v ball_pos new_ball_pos in
  if Segment.intersect ball_segment right then goal ~side:Left state
  else if Segment.intersect ball_segment left then goal ~side:Right state
  else if Segment.intersect ball_segment (player_segment ~side:Left state) then
    player_collision ~side:Left ~player_speed:player_left_speed state
  else if Segment.intersect ball_segment (player_segment ~side:Right state) then
    player_collision ~side:Right ~player_speed:player_right_speed state
  else
    let state =
      [top; bottom]
      |> List.find_opt (Segment.intersect ball_segment)
      |> Option.map (wall_collision state)
      |> Option.value ~default:{state with ball_pos= new_ball_pos}
    in
    state

let color = Color.white

let draw_background ~io = fill_rect ~io ~color:Color.black box_game

let draw_court ~io = draw_rect ~io ~color court

let draw_score ~io ~state =
  let { player_left= {score= score_left; _}
      ; player_right= {score= score_right; _}
      ; _ } =
    state
  in
  let score_left = string_of_int score_left in
  let score_right = string_of_int score_right in
  draw_string ~io ~color ~size:18 score_left
    (V2.v (Box.ox box_game +. 10.) (Box.oy court -. 22.)) ;
  draw_string ~io ~color ~size:18 score_right
    (V2.v (Box.midx box_game +. 10.) (Box.oy court -. 22.))

let draw_ball ~io {ball_pos; _} = fill_circle ~io ~color (Circle.v ball_pos 4.)

let draw_players ~io state =
  let player_left = player_segment ~side:Left state
  and player_right = player_segment ~side:Right state in
  draw_line ~io ~color player_left ;
  draw_line ~io ~color player_right

let update ~io state =
  let io = View.drawing_box box_game io in
  let player_speed = player_speed state in
  if Event.is_pressed ~io `escape then raise Exit ;
  let player_left_speed, state =
    if Event.is_pressed ~io (`char 'w') then
      let delta_y = -.player_speed in
      let player_left =
        update_player ~x:player_left_x ~player:state.player_left ~delta_y
      in
      (delta_y, {state with player_left})
    else if Event.is_pressed ~io (`char 's') then
      let delta_y = player_speed in
      let player_left =
        update_player ~x:player_left_x ~player:state.player_left ~delta_y
      in
      (delta_y, {state with player_left})
    else (0., state)
  in
  let player_right_speed, state =
    if Event.is_pressed ~io `arrow_up then
      let delta_y = -.player_speed in
      let player_right =
        update_player ~x:player_right_x ~player:state.player_right ~delta_y
      in
      (delta_y, {state with player_right})
    else if Event.is_pressed ~io `arrow_down then
      let delta_y = player_speed in
      let player_right =
        update_player ~x:player_right_x ~player:state.player_right ~delta_y
      in
      (delta_y, {state with player_right})
    else (0., state)
  in
  let state = tick ~player_left_speed ~player_right_speed state in
  draw_background ~io ;
  draw_court ~io ;
  draw_ball ~io state ;
  draw_players ~io state ;
  draw_score ~io ~state ;
  state

let () = run init update
