open Draw_geometry
open Ui_backend
open Widget_builder

type params = slider_params
type state = slider_state = { v : float; grasped : bool }
type return = float
type Ui_backend.state += Slider of state

let construct_state s = Slider s
let destruct_state s = match s with Slider s -> s | _ -> raise IdTypeMismatch

let size ~ts:_ { w; min = _; max = _ } =
  let height = 20. in
  Size.v (w +. (2. *. padding)) height

let render ~io { w = _; min; max } state box =
  let radius = 8. in
  let w = Box.w box -. (2. *. padding) in
  let sval = state.v in
  let line = Box.v_mid (Box.mid box) (Size.v w 4.) in
  Box.fill ~io ~color:lowlight line;
  let pos =
    radius +. ((sval -. min) *. (w -. (2. *. radius)) /. (max -. min))
  in
  Box.fill ~io ~color:highlight (Box.v (Box.o line) (Size.v pos 4.));
  Circle.fill ~io ~color:highlight
    (Circle.v (Point.v (Box.minx line +. pos) (Box.midy line)) radius)

let update ~io { w = _; min; max } state box =
  let w = Box.w box -. (2. *. padding) in
  let { v; grasped } = state in
  let grasped =
    if grasped then not (View.clip_events false ~io @@ Event.is_up `click_left)
    else Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box
  in
  let v =
    if grasped then
      Float.max min @@ Float.min max
      @@ ((Vec.x (Event.mouse_pos ~io) -. Box.minx box) *. (max -. min) /. w)
         +. min
    else v
  in
  { v; grasped }

let result _ state = state.v

let v =
  elt ~construct_state ~destruct_state
    ~default:(fun { max; _ } -> { v = max; grasped = false })
    ~size ~render ~update ~result ()
