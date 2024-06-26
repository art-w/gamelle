open Geometry

type key =
  [ `alt
  | `alt_gr
  | `arrow_down
  | `arrow_left
  | `arrow_right
  | `arrow_up
  | `backspace
  | `click_left
  | `click_right
  | `control_left
  | `control_right
  | `delete
  | `escape
  | `input_char of string
  | `meta
  | `physical_char of char
  | `quit
  | `shift
  | `space
  | `tab
  | `wheel
  | `unknown_key ]

module Strings = Set.Make (String)

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
  pressed_chars : Strings.t;
  down_chars : Strings.t;
  up_chars : Strings.t;
  clock : int;
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
    pressed_chars = Strings.empty;
    down_chars = Strings.empty;
    up_chars = Strings.empty;
    clock = 0;
  }

let desired_fps = 60.0
let desired_dt = 1. /. desired_fps
let dt (_ : t) = desired_dt
let clock t = float t.clock /. desired_fps
let insert = Keys.add
let remove = Keys.remove
let diff = Keys.diff
let union = Keys.union
let is_pressed t key = Keys.mem key t.keypressed
let is_up t key = Keys.mem key t.keyup
let is_down t key = Keys.mem key t.keydown

let update_updown previous t =
  let keyup = Keys.diff previous.keypressed t.keypressed in
  let keydown = Keys.diff t.keypressed previous.keypressed in
  let up_chars = Strings.diff previous.pressed_chars t.pressed_chars in
  let down_chars = Strings.diff t.pressed_chars previous.pressed_chars in
  { t with keyup; keydown; up_chars; down_chars }

let wheel_delta t = t.wheel_delta

let reset_wheel t =
  { t with keypressed = remove `wheel t.keypressed; wheel_delta = 0. }
