open! Common
include Gamelle_common.Event

let key_of_keycode kc =
  match () with
  | _ when kc = Sdl.K.escape -> Escape
  | _ when kc = Sdl.K.lctrl -> Control_left
  | _ when kc = Sdl.K.rctrl -> Control_right
  | _ when kc = Sdl.K.left -> Arrow_left
  | _ when kc = Sdl.K.right -> Arrow_right
  | _ when kc = Sdl.K.up -> Arrow_up
  | _ when kc = Sdl.K.down -> Arrow_down
  | _ when kc = Sdl.K.rctrl -> Control_right
  | _ when kc >= Sdl.K.a && kc < Sdl.K.z ->
      Char (Char.chr (Char.code 'a' + kc - Sdl.K.a))
  | _ -> failwith (Printf.sprintf "unhandled %#i" kc)

let key_of_event e = key_of_keycode (Sdl.Event.get e Sdl.Event.keyboard_keycode)

let update t e =
  let typ = Sdl.Event.get e Sdl.Event.typ in
  match () with
  | _ when typ = Sdl.Event.quit -> raise Exit
  | _ when typ = Sdl.Event.key_down ->
      let key = key_of_event e in
      { t with keypressed = key :: t.keypressed }
  | _ when typ = Sdl.Event.key_up ->
      let key = key_of_event e in
      { t with keypressed = List.filter (( <> ) key) t.keypressed }
  | _ ->
      (* Format.printf "unhandled event@." ; *)
      t

let rec insert v = function
  | [] -> [ v ]
  | x :: xs when x = v -> x :: xs
  | x :: xs -> x :: insert v xs

let rec remove v = function
  | [] -> []
  | x :: xs when x = v -> xs
  | x :: xs -> x :: remove v xs

let update_mouse t =
  let state, (x, y) = Sdl.get_mouse_state () in
  let t =
    if Int32.logand state Sdl.Button.lmask <> Int32.zero then
      { t with keypressed = insert Click_left t.keypressed }
    else { t with keypressed = remove Click_left t.keypressed }
  in
  let t =
    if Int32.logand state Sdl.Button.rmask <> Int32.zero then
      { t with keypressed = insert Click_right t.keypressed }
    else { t with keypressed = remove Click_right t.keypressed }
  in
  { t with mouse_x = float x; mouse_y = float y }

let is_pressed t key = List.mem key t.keypressed
