open Draw_geometry
open Ui_backend
open Widget_builder

type params = { w : float; min : int; max : int }
type state = { v : int; grasped : bool }
type return = float
type Ui_backend.state += Slider of state

let construct_state s = Slider s
let destruct_state s = match s with Slider s -> s | _ -> raise IdTypeMismatch

let size ~ts:_ { w; min = _; max = _ } =
  let height = 20. in
  Size.v (w +. (2. *. padding)) height

let render ~io { w = _; min; max } state box =
  let radius = 8. in
  let w = Box.width box -. (2. *. padding) in
  let sval = state.v in
  let line = Box.v_center (Box.center box) (Size.v w 4.) in
  Box.fill ~io ~color:lowlight line;
  let min = float_of_int min
  and max = float_of_int max
  and sval = float_of_int sval in
  let pos =
    radius +. ((sval -. min) *. (w -. (2. *. radius)) /. (max -. min))
  in
  Box.fill ~io ~color:highlight (Box.v (Box.top_left line) (Size.v pos 4.));
  Circle.fill ~io ~color:highlight
    (Circle.v (Point.v (Box.x_left line +. pos) (Box.y_middle line)) radius)

let update ~io { w = _; min; max } state box =
  let w = Box.width box -. (2. *. padding) in
  let { v; grasped } = state in
  let grasped =
    if grasped then
      let io = View.clip_events false io in
      not (Event.is_up ~io `click_left)
    else Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box
  in
  let v =
    if grasped then
      let min = float_of_int min and max = float_of_int max in
      int_of_float
        (Float.max min @@ Float.min max
        @@ (((Event.mouse_pos ~io).x -. Box.x_left box) *. (max -. min) /. w)
           +. min)
    else v
  in
  { v; grasped }

let destruct_result _ n = { v = n; grasped = false }
let result _ state = state.v

let v =
  elt ~construct_state ~destruct_state
    ~default:(fun { max; _ } -> { v = max; grasped = false })
    ~size ~render ~update ~destruct_result ~result ()
