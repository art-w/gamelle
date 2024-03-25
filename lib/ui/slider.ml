open Gamelle_backend
open Geometry
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
  Size2.v (w +. (2. *. padding)) height

let render ~io { w = _; min; max } state box =
  let w = Box.w box in
  let sval = state.v in
  let line = Box.v_mid (Box.mid box) (Size2.v w 4.) in
  fill_rect ~io ~color:lowlight line;
  let pos = (sval -. min) *. Box.w box /. (max -. min) in
  fill_rect ~io ~color:highlight (Box.v (Box.o line) (Size2.v pos 4.));
  fill_circle ~io ~color:highlight
    (Circle.v (P2.v (Box.minx line +. pos) (Box.midy line)) 8.)

let update ~io { w = _; min; max } state box =
  let { v; grasped } = state in
  let grasped =
    if grasped then not (View.clip_events false ~io @@ Event.is_up `click_left)
    else Event.is_pressed ~io `click_left && Box.mem (Event.mouse_pos ~io) box
  in
  let v =
    if grasped then
      Float.max min @@ Float.min max
      @@ (V2.x (Event.mouse_pos ~io) -. Box.minx box)
         *. (max -. min) /. Box.w box
         +. min
    else v
  in
  { v; grasped }

let result state = state.v

let v =
  elt ~construct_state ~destruct_state
    ~default:(fun { max; _ } -> { v = max; grasped = false })
    ~size ~render ~update ~result
