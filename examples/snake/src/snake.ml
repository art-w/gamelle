open Gamelle

let () = Random.init 1
let size = 42
let cell_size = 10.0

type xy = int * int

let ( ++ ) (x, y) (dx, dy) = (x + dx, y + dy)
let random_xy () = (Random.int size, Random.int size)
let is_outside (x, y) = x < 0 || y < 0 || x >= size || y >= size

type snake = { dir : xy; cells : xy list; growth : int }
type state = { apple : xy; snake : snake; last_frame : float }

let cell (x, y) =
  let f v = cell_size *. float v in
  Box.v (Point.v (f x) (f y)) (Size.v cell_size cell_size)

let head snake = List.hd snake.cells
let is_colliding head body = List.mem head body

let update snake =
  let new_head = head snake ++ snake.dir in
  let growth, body =
    if snake.growth > 0 then (snake.growth - 1, snake.cells)
    else (0, List.rev (List.tl (List.rev snake.cells)))
  in
  if is_outside new_head || is_colliding new_head body then snake
  else { snake with cells = new_head :: body; growth }

let frame_duration state =
  let len = float (List.length state.snake.cells) in
  let max_len = float (size * size / 10) in
  let ratio = Ease.out_quad (min 1.0 (len /. max_len)) in
  let min_speed = 0.08 in
  let max_speed = 0.03 in
  min_speed +. (ratio *. (max_speed -. min_speed))

let rec random_apple snake =
  let apple = random_xy () in
  if is_colliding apple snake.cells then random_apple snake else apple

let init_snake = { dir = (1, 0); cells = [ (size / 2, size / 2) ]; growth = 5 }

let init =
  { apple = random_apple init_snake; snake = init_snake; last_frame = 0. }

let () =
  Gamelle.run init @@ fun ~io state ->
  if Input.is_pressed ~io `escape then raise Exit;

  let state = if Input.is_pressed ~io `click_left then init else state in

  let state =
    let snake = state.snake in
    let dir =
      if Input.is_pressed ~io `arrow_left then (-1, 0)
      else if Input.is_pressed ~io `arrow_right then (1, 0)
      else if Input.is_pressed ~io `arrow_down then (0, 1)
      else if Input.is_pressed ~io `arrow_up then (0, -1)
      else snake.dir
    in
    let snake = { snake with dir } in
    { state with snake }
  in

  let state =
    if state.last_frame +. frame_duration state > clock ~io then state
    else
      let snake = update state.snake in
      let snake, apple =
        if head snake = state.apple then
          ({ snake with growth = snake.growth + 5 }, random_apple snake)
        else (snake, state.apple)
      in
      { snake; apple; last_frame = clock ~io }
  in

  let width = cell_size *. float size in
  Window.set_size ~io (Size.v (width +. 20.0) (60.0 +. width));
  Box.fill ~io ~color:Color.black (Window.box ~io);
  let io = View.translate (Vec.v 10.0 10.0) io in
  draw_string ~io ~at:Point.o
    (Printf.sprintf "Score: %i" (List.length state.snake.cells));
  let io = View.translate (Vec.v 0.0 40.0) io in
  Box.draw ~io (Box.v Point.o (Size.v width width));
  Box.fill ~io:(View.color Color.red io) (cell state.apple);
  List.iter (fun at -> Box.fill ~io (cell at)) state.snake.cells;
  let grey = Color.v 0.1 0.1 0.0 0.4 in
  for x = 0 to size do
    let x = cell_size *. float x in
    Segment.draw ~io ~color:grey (Segment.v (Point.v x 0.0) (Point.v x width));
    Segment.draw ~io ~color:grey (Segment.v (Point.v 0.0 x) (Point.v width x))
  done;

  state
