type key =
  | Escape
  | Control_left
  | Control_right
  | Arrow_left
  | Arrow_right
  | Arrow_up
  | Arrow_down
  | Char of char
  | Click_left
  | Click_right
  | Wheel_up
  | Wheel_down

module Keys = Set.Make (struct
  type t = key

  let compare = Stdlib.compare
end)

type t = {
  keyup : Keys.t;
  keydown : Keys.t;
  keypressed : Keys.t;
  mouse_x : float;
  mouse_y : float;
}

let mouse_pos t = (t.mouse_x, t.mouse_y)

let default =
  {
    keyup = Keys.empty;
    keydown = Keys.empty;
    keypressed = Keys.empty;
    mouse_x = 0.0;
    mouse_y = 0.0;
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
