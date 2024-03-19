open Brr
include Gamelle_common.Event

let previous = ref default
let current = ref default

let new_frame () =
  current := Gamelle_common.Event.update_updown !previous !current;
  previous := !current
let key_of_keycode kc =
  match Jstr.to_string kc with
  | "ArrowLeft" -> `arrow_left
  | "ArrowRight" -> `arrow_right
  | "ArrowUp" -> `arrow_up
  | "ArrowDown" -> `arrow_down
  | "Escape" -> `escape
  | "ControlLeft" -> `control_left
  | "ControlRight" -> `control_right
  | key when String.length key = 4 && String.starts_with ~prefix:"Key" key ->
      let lt = key.[3] in
      `char (Char.lowercase_ascii lt)
  | kc ->
      Console.(log [ "TODO key:"; kc ]);
      `unknown_key

let key_of_event e = key_of_keycode (Ev.Keyboard.code e)

let update ~status t e =
  let key = key_of_event e in
  match status with
  | `Up -> { t with keypressed = remove key t.keypressed }
  | `Down -> { t with keypressed = insert key t.keypressed }

let do_update ~status e = current := update ~status !current (Ev.as_type e)

let update_mouse t e =
  let x, y = (Ev.Mouse.offset_x e, Ev.Mouse.offset_y e) in
  let buttons = Ev.Mouse.buttons e in
  let t =
    if buttons land 0x01 <> 0 then
      { t with keypressed = insert `click_left t.keypressed }
    else { t with keypressed = remove `click_left t.keypressed }
  in
  let t =
    if buttons land 0x10 <> 0 then
      { t with keypressed = insert `click_right t.keypressed }
    else { t with keypressed = remove `click_right t.keypressed }
  in
  { t with mouse_x = x; mouse_y = y }

let do_update_mouse e = current := update_mouse !current (Ev.as_type e)

let update_wheel t e =
  let delta = Ev.Wheel.delta_y e /. 4. in
  { t with keypressed = insert `wheel t.keypressed; wheel_delta = delta }

let do_update_wheel e = current := update_wheel !current (Ev.as_type e)

let attach ~target =
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "keyup"))
      (do_update ~status:`Up) target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "keydown"))
      (do_update ~status:`Down) target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousemove"))
      do_update_mouse target
  in
  let _ =
    Ev.listen (Ev.Type.create (Jstr.of_string "mouseup")) do_update_mouse target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousedown"))
      do_update_mouse target
  in
  let _ =
    Ev.listen (Ev.Type.create (Jstr.of_string "wheel")) do_update_wheel target
  in
  ()
