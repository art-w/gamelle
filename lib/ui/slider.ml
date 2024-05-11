open Draw_geometry
open Ui_backend
open Widgets

let radius = 8.
let r2 = 2.0 *. radius

let render ~io percent box =
  let w = Box.width box in
  let line = Box.v_center (Box.center box) (Size.v w 4.) in
  Box.fill ~io ~color:lowlight line;
  let pos = radius +. (percent *. (w -. (2. *. radius))) in
  Box.fill ~io ~color:highlight (Box.v (Box.top_left line) (Size.v pos 4.));
  Circle.fill ~io ~color:highlight
    (Circle.v (Point.v (Box.x_left line +. pos) (Box.y_middle line)) radius)

let with_horizontal_drag ui box percent fn =
  on_click ui @@ fun state ->
  let percent =
    match state with
    | `normal | `hover -> percent
    | `pressed | `clicked ->
        let m = Event.mouse_pos ~io:(get_io ui) in
        (m.x -. Box.x_left box) /. Box.width box
  in
  let percent = max 0.0 (min 1.0 percent) in
  fn percent

let v ui ~min:min_value ~max:max_value value =
  with_box ui @@ fun box ->
  let box = Box.shrink ~left:radius ~right:radius box in
  let range = max_value -. min_value in
  let range = if range <= 0.0 then 1.0 else range in
  let percent = (value -. min_value) /. range in
  with_horizontal_drag ui box percent @@ fun percent ->
  let value = min_value +. (percent *. range) in
  draw ui ~min_width:r2 ~min_height:r2 ~flex_width:1.0 (render percent);
  value
