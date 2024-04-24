open Draw_geometry
open Ui_backend
open Widget_builder

type 'a params = 'a vscroll_params
type state = vscroll_state
type Ui_backend.state += VScroll of state

let construct_state s = VScroll s
let destruct_state s = match s with VScroll s -> s | _ -> raise IdTypeMismatch
let scroll_bar_width = 10.

let size ~ts:_ ~children_size { height; f = _ } =
  let height = height in
  let width =
    (* Float.max space_available *)
    Size.w children_size +. scroll_bar_width
  in
  Size.(v width height)

let render ~io { height = _; f = _ } state box =
  let height = Box.h box in
  let { size = _; offset; grasped = _; real_height } = state in
  let scroll_rail_box =
    Box.v
      (Point.v (Box.maxx box -. scroll_bar_width) (Box.miny box))
      (Size.v scroll_bar_width height)
  in
  let scroll_bar_height = height *. height /. real_height in
  let scroll_bar_box =
    Box.v
      (Point.v
         (Box.maxx box -. scroll_bar_width)
         ((offset *. height /. state.real_height) +. Box.miny box))
      (Size.v scroll_bar_width scroll_bar_height)
  in
  Box.draw ~io ~color:fg box;
  Box.fill ~io ~color:lowlight scroll_rail_box;
  Box.fill ~io ~color:highlight scroll_bar_box

let update ~io ~children_size box state { height = _; f = _ } =
  let { size; offset; grasped; real_height = _ } = state in
  let height = Box.h box in
  let real_height = Size.h children_size in
  let scroll_rail_box =
    Box.v
      (Point.v (Box.maxx box -. scroll_bar_width) (Box.miny box))
      (Size.v scroll_bar_width height)
  in
  let scroll_bar_height = height *. height /. real_height in
  let max_offset = real_height -. height in
  let mouse_pos = Event.mouse_pos ~io in
  let grasped =
    if grasped then not (View.clip_events false ~io @@ Event.is_up `click_left)
    else Event.is_down ~io `click_left && Box.mem mouse_pos scroll_rail_box
  in
  let offset =
    max 0. @@ min max_offset
    @@
    if grasped then
      max_offset
      *. (height /. (height -. scroll_bar_height))
      *. (Point.y mouse_pos -. Box.miny scroll_rail_box)
      /. height
      -. (height /. 2.)
    else if Event.is_pressed ~io `wheel then
      let amount = Event.wheel_delta ~io in
      offset +. amount
    else offset
  in
  { size; offset; grasped; real_height }

let result { height = _; f } = f ()

let default _ =
  { size = Size.zero; offset = 0.; grasped = false; real_height = 0. }

let size_for_self = Size.(v scroll_bar_width 0.)
let children_offset state = Vec.(zero - v 0. state.offset)
let children_io ~io box = View.clipped_events true @@ View.clipped box io

let v : type a. (_, a params, a) node =
 fun (ui, loc) ?id ?(style = Style.default) ?(size = size) ?(render = render)
     params ->
  node ~construct_state ~destruct_state ~dir:V ~default ~size ~size_for_self
    ~children_io ~children_offset ~render ~update ~result () (ui, loc) ?id
    ~style ~size ~render params
