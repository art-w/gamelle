open Brr
include Gamelle_common.Event

let previous = ref default
let current = ref default

let new_frame () =
  current := Gamelle_common.Event.update_updown !previous !current;
  previous := !current

let key_of_keycode kc =
  match Jstr.to_string kc with
  | "ArrowLeft" -> Arrow_left
  | "ArrowRight" -> Arrow_right
  | "ArrowUp" -> Arrow_up
  | "ArrowDown" -> Arrow_down
  | "Escape" -> Escape
  | "ControlLeft" -> Control_left
  | "ControlRight" -> Control_right
  | key when String.length key = 4 && String.starts_with ~prefix:"Key" key ->
      let lt = key.[3] in
      Char (Char.lowercase_ascii lt)
  | kc ->
      Console.(log [ "TODO key:"; kc ]);
      Escape

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
      { t with keypressed = insert Click_left t.keypressed }
    else { t with keypressed = remove Click_left t.keypressed }
  in
  let t =
    if buttons land 0x10 <> 0 then
      { t with keypressed = insert Click_right t.keypressed }
    else { t with keypressed = remove Click_right t.keypressed }
  in
  { t with mouse_x = x; mouse_y = y }

let do_update_mouse e = current := update_mouse !current (Ev.as_type e)

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
  ()
