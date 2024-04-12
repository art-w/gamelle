open Geometry
type key =
  [ `quit
  | `escape
  | `control_left
  | `control_right
  | `arrow_left
  | `arrow_right
  | `arrow_up
  | `arrow_down
  | `char of char
  | `click_left
  | `click_right
  | `wheel
  | `unknown_key ]

module Keys = Set.Make (struct
  type t = key

  let compare a b = Stdlib.compare a b
end)

type t = {
  keyup : Keys.t;
  keydown : Keys.t;
  keypressed : Keys.t;
  mouse_x : float;
  mouse_y : float;
  wheel_delta : float;
}

let mouse_pos t = Point.v t.mouse_x t.mouse_y

let default =
  {
    keyup = Keys.empty;
    keydown = Keys.empty;
    keypressed = Keys.empty;
    mouse_x = 0.0;
    mouse_y = 0.0;
    wheel_delta = 0.;
  }

let insert = Keys.add
let remove = Keys.remove
let is_pressed t key = Keys.mem key t.keypressed
let is_up t key = Keys.mem key t.keyup
let is_down t key = Keys.mem key t.keydown

let update_updown previous t =
  let keyup = Keys.diff previous.keypressed t.keypressed in
  let keydown = Keys.diff t.keypressed previous.keypressed in
  { t with keyup; keydown }

let wheel_delta t = t.wheel_delta

let reset_wheel t =
  { t with keypressed = remove `wheel t.keypressed; wheel_delta = 0. }
