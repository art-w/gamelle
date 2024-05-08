open! Common
open Gamelle_common.Events_backend

let key_of_keycode kc =
  match () with
  | _ when kc = Sdl.K.lalt -> [ `alt ]
  | _ when kc = Sdl.K.ralt -> [ `alt_gr ]
  | _ when kc = Sdl.K.down -> [ `arrow_down ]
  | _ when kc = Sdl.K.left -> [ `arrow_left ]
  | _ when kc = Sdl.K.right -> [ `arrow_right ]
  | _ when kc = Sdl.K.up -> [ `arrow_up ]
  | _ when kc = Sdl.K.backspace -> [ `backspace ]
  | _ when kc = Sdl.K.lctrl -> [ `control_left ]
  | _ when kc = Sdl.K.rctrl -> [ `control_right ]
  | _ when kc = Sdl.K.delete -> [ `delete ]
  | _ when kc = Sdl.K.escape -> [ `escape ]
  (* todo : meta key *)
  | _ when kc = Sdl.K.lshift -> [ `shift ]
  | _ when kc = Sdl.K.rshift -> [ `shift ]
  | _ when kc = Sdl.K.space -> [ `space ]
  | _ when kc = Sdl.K.tab -> [ `tab ]
  | _ -> []

let key_of_scancode sc =
  let module Sc = Sdl.Scancode in
  match () with
  | _ when sc >= Sc.a && sc <= Sc.z ->
      let char = Char.(chr (code 'a' + sc - Sc.a)) in
      [ `physical_char char ]
  | _ -> []

let keys_of_event e =
  Keys.of_list
  @@ key_of_scancode Sdl.Event.(get e keyboard_scancode)
  @ key_of_keycode (Sdl.Event.get e Sdl.Event.keyboard_keycode)

let update t e =
  let typ = Sdl.Event.get e Sdl.Event.typ in
  let t =
    {
      t with
      pressed_chars = Strings.empty;
      keypressed =
        Keys.filter (function `input_char _ -> false | _ -> true) t.keypressed;
    }
  in
  match () with
  | _ when typ = Sdl.Event.quit ->
      { t with keypressed = insert `quit t.keypressed }
  | _ when typ = Sdl.Event.text_input ->
      let char = Sdl.Event.(get e text_input_text) in
      let keypressed = Keys.add (`input_char char) t.keypressed in
      let pressed_chars = Strings.singleton char in
      { t with pressed_chars; keypressed }
  | _ when typ = Sdl.Event.key_down ->
      let keys = keys_of_event e in
      { t with keypressed = Keys.union keys t.keypressed }
  | _ when typ = Sdl.Event.key_up ->
      let keys = keys_of_event e in
      { t with keypressed = Keys.diff t.keypressed keys }
  | _ when typ = Sdl.Event.mouse_wheel -> (
      let wheel_delta = Sdl.Event.(get e mouse_wheel_y) in
      let wheel_delta = t.wheel_delta +. float (wheel_delta * 4) in
      let t = { t with wheel_delta } in
      match wheel_delta with
      | d when d <> 0. ->
          { t with keypressed = insert `wheel @@ remove `wheel t.keypressed }
      | _ -> { t with keypressed = remove `wheel t.keypressed })
  | _ ->
      (* Format.printf "unhandled event@." ; *)
      t

let update t e = try update t e with Exit as exn -> raise exn | _ -> t

let reset ~now t =
  {
    t with
    clock = now;
    keypressed = remove `wheel t.keypressed;
    wheel_delta = 0.;
  }

let update_mouse t =
  let state, (x, y) = Sdl.get_mouse_state () in
  let t =
    if Int32.logand state Sdl.Button.lmask <> Int32.zero then
      { t with keypressed = insert `click_left t.keypressed }
    else { t with keypressed = remove `click_left t.keypressed }
  in
  let t =
    if Int32.logand state Sdl.Button.rmask <> Int32.zero then
      { t with keypressed = insert `click_right t.keypressed }
    else { t with keypressed = remove `click_right t.keypressed }
  in
  { t with mouse_x = float x; mouse_y = float y }

let update ~clock previous =
  let t = reset ~now:clock previous in
  let e = Sdl.Event.create () in
  let rec update_poll t =
    if Sdl.poll_event (Some e) then
      let t = update t e in
      let t = update_mouse t in
      update_poll t
    else t
  in
  let t = update_poll t in
  update_updown previous t
