open Gamelle_common.Events_backend

let uchar_to_utf8 u =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let physical_key_pairs =
  [
    (Raylib.Key.A, 'a');
    (Raylib.Key.B, 'b');
    (Raylib.Key.C, 'c');
    (Raylib.Key.D, 'd');
    (Raylib.Key.E, 'e');
    (Raylib.Key.F, 'f');
    (Raylib.Key.G, 'g');
    (Raylib.Key.H, 'h');
    (Raylib.Key.I, 'i');
    (Raylib.Key.J, 'j');
    (Raylib.Key.K, 'k');
    (Raylib.Key.L, 'l');
    (Raylib.Key.M, 'm');
    (Raylib.Key.N, 'n');
    (Raylib.Key.O, 'o');
    (Raylib.Key.P, 'p');
    (Raylib.Key.Q, 'q');
    (Raylib.Key.R, 'r');
    (Raylib.Key.S, 's');
    (Raylib.Key.T, 't');
    (Raylib.Key.U, 'u');
    (Raylib.Key.V, 'v');
    (Raylib.Key.W, 'w');
    (Raylib.Key.X, 'x');
    (Raylib.Key.Y, 'y');
    (Raylib.Key.Z, 'z');
  ]

let special_key_pairs =
  [
    (Raylib.Key.Left_alt, `alt);
    (Raylib.Key.Right_alt, `alt_gr);
    (Raylib.Key.Down, `arrow_down);
    (Raylib.Key.Left, `arrow_left);
    (Raylib.Key.Right, `arrow_right);
    (Raylib.Key.Up, `arrow_up);
    (Raylib.Key.Backspace, `backspace);
    (Raylib.Key.Left_control, `control_left);
    (Raylib.Key.Right_control, `control_right);
    (Raylib.Key.Delete, `delete);
    (Raylib.Key.Escape, `escape);
    (Raylib.Key.Left_super, `meta);
    (Raylib.Key.Right_super, `meta);
    (Raylib.Key.Left_shift, `shift);
    (Raylib.Key.Right_shift, `shift);
    (Raylib.Key.Space, `space);
    (Raylib.Key.Tab, `tab);
  ]

let build_keypressed () =
  let keys = ref Keys.empty in
  physical_key_pairs
  |> List.iter begin fun (rk, c) ->
      if Raylib.is_key_down rk then keys := Keys.add (`physical_char c) !keys
    end;
  special_key_pairs
  |> List.iter begin fun (rk, k) ->
      if Raylib.is_key_down rk then keys := Keys.add k !keys
    end;
  if Raylib.is_mouse_button_down Raylib.MouseButton.Left then
    keys := Keys.add `click_left !keys;
  if Raylib.is_mouse_button_down Raylib.MouseButton.Right then
    keys := Keys.add `click_right !keys;
  let wheel = Raylib.get_mouse_wheel_move () in
  if wheel <> 0. then keys := Keys.add `wheel !keys;
  !keys

let rec collect_chars acc =
  let u = Raylib.get_char_pressed () in
  if Uchar.to_int u = 0 then acc
  else collect_chars (Strings.add (uchar_to_utf8 u) acc)

let update clock previous =
  let wheel_delta = Raylib.get_mouse_wheel_move () *. 4.0 in
  let pos = Raylib.get_mouse_position () in
  let mouse_x = Raylib.Vector2.x pos in
  let mouse_y = Raylib.Vector2.y pos in
  let pressed_chars = collect_chars Strings.empty in
  let keypressed = build_keypressed () in
  let keypressed =
    if Raylib.window_should_close () then Keys.add `quit keypressed
    else keypressed
  in
  let keypressed =
    Strings.fold
      (fun s k -> Keys.add (`input_char s) k)
      pressed_chars keypressed
  in
  let t =
    {
      previous with
      clock;
      mouse_x;
      mouse_y;
      wheel_delta;
      pressed_chars;
      keypressed;
    }
  in
  update_updown previous t
