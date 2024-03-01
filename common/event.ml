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

type t = { keypressed : key list; mouse_x : float; mouse_y : float }

let mouse_pos t = (t.mouse_x, t.mouse_y)
let default = { keypressed = []; mouse_x = 0.0; mouse_y = 0.0 }
