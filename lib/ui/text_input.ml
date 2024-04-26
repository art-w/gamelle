open Draw_geometry
open Ui_backend
open Widget_builder

type params = float

type state = {
  text : Text.t;
  offset : float;
  cursor : int;
  focused : bool;
  pressed_key : (int * Event.key) option;
}

type return = string
type Ui_backend.state += Text_input of state

let default _ =
  {
    text = Text.of_string "";
    offset = 0.;
    cursor = 0;
    focused = false;
    pressed_key = None;
  }

(*

let rec v1 = body1
and v2 = body2

---
body2
---
( let rec v1 = body1 in
  and v2 = body2 in
  v2
)
---
let module M = struct
  let rec v1 = body1
  and v2 = body2
end
in
M.v2

  *)

let construct_state s = Text_input s

let destruct_state s =
  match s with Text_input b -> b | _ -> raise IdTypeMismatch

let size ~ts width =
  let text_size = Size.(v width (h (ts "a"))) in
  Vec.(text_size + (2. * padding_xy))

let text_length ~io text = Size.w (Text.size_t ~io ~size:font_size text)
let cursor_offset ~io text cursor = text_length ~io (Text.sub text 0 cursor)

let delete_char i text =
  Text.(sub text 0 i ^ sub text (i + 1) (length text - (i + 1)))

let render ~io _params { text; offset; cursor; focused; pressed_key = _ } box =
  Box.fill ~io ~color:bg' box;
  Box.draw ~io ~color:(if focused then highlight else fg) box;
  let io = View.clipped box io in
  let nsize = Vec.(Box.size box - (2. * padding_xy)) in
  let box = Box.(v_mid (mid box) nsize) in
  Text.draw_t ~io ~color:fg ~size:font_size text Vec.(Box.o box + v offset 0.);
  let cursor_offset = cursor_offset ~io text cursor in
  let cursor_pos = cursor_offset +. offset in
  let cursor_seg =
    Segment.v
      (Point.v (Box.minx box +. cursor_pos) (Box.miny box))
      (Point.v
         (Box.minx box +. cursor_pos)
         (Box.miny box +. Size.h (Text.size ~io ~size:font_size "a")))
  in
  if focused then Segment.draw ~io ~color:highlight cursor_seg

let find_cursor_click ~io text x =
  let rec loop prev_w i =
    if i >= Text.length text then i
    else
      let subtext = Text.sub text 0 i in
      let w = text_length ~io subtext in
      if w > x then if w -. x < x -. prev_w then i else i - 1 else loop w (i + 1)
  in
  loop 0. 0

let slow_frames = 60
let slow_frequency = 20
let fast_frequency = 3

let update ~io _param { text; offset; cursor; focused; pressed_key } box =
  let nsize = Vec.(Box.size box - (2. * padding_xy)) in
  let box = Box.(v_mid (mid box) nsize) in
  let width = max (Box.w box) 0. in
  let pressed_key =
    match pressed_key with
    | Some (_, key) when Event.is_up ~io key -> None
    | k -> k
  in
  let pressed_key =
    Option.map (fun (frames, k) -> (frames + 1, k)) pressed_key
  in
  let pressed_key =
    if Event.is_down ~io `arrow_left then Some (0, `arrow_left)
    else if Event.is_down ~io `arrow_right then Some (0, `arrow_right)
    else if Event.is_down ~io `backspace then Some (0, `backspace)
    else pressed_key
  in
  let module Strings = Event.Strings in
  let pressed_key =
    let char_down = Event.down_chars ~io |> Strings.choose_opt in
    match char_down with
    | Some c -> Some (0, `input_char c)
    | None -> pressed_key
  in
  let cursor, focused =
    if is_clicked ~io box then
      let x = (Point.x @@ Event.mouse_pos ~io) -. Box.minx box +. offset in
      (find_cursor_click ~io text x, true)
    else if clicked_outside ~io box then (cursor, false)
    else (cursor, focused)
  in
  let cursor =
    Int.max 0
      (Int.min (Text.length text)
         (match pressed_key with
         | None -> cursor
         | Some (0, ((`arrow_left | `arrow_right) as dir)) -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | Some (n, ((`arrow_left | `arrow_right) as dir))
           when n < slow_frames && n mod slow_frequency = 0 -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | Some (n, ((`arrow_left | `arrow_right) as dir))
           when n >= slow_frames && n mod fast_frequency = 0 -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | _ -> cursor))
  in
  let char_to_add =
    match pressed_key with
    | None -> None
    | Some (0, `input_char char) -> Some char
    | Some (n, `input_char char)
      when n < slow_frames && n mod slow_frequency = 0 ->
        Some char
    | Some (n, `input_char char)
      when n >= slow_frames && n mod fast_frequency = 0 ->
        Some char
    | _ -> None
  in
  let cursor, text =
    match char_to_add with
    | None -> (cursor, text)
    | Some char ->
        ( cursor + 1,
          Text.(
            sub text 0 cursor ^ of_string char
            ^ sub text cursor (length text - cursor)) )
  in
  let cursor, text =
    match pressed_key with
    | None -> (cursor, text)
    | Some (0, `backspace) when cursor > 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | Some (n, `backspace)
      when cursor > 0 && n < slow_frames && n mod slow_frequency = 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | Some (n, `backspace)
      when cursor > 0 && n >= slow_frames && n mod fast_frequency = 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | _ -> (cursor, text)
  in

  let cursor_pos = cursor_offset ~io text cursor +. offset in
  let offset =
    if cursor_pos < 0. then offset +. text_length ~io (Text.sub text cursor 1)
    else if cursor_pos > width then
      let char_text = Text.sub text (cursor - 1) 1 in
      let char_len = text_length ~io char_text in
      offset -. char_len
    else offset
  in
  { text; offset; cursor; focused; pressed_key }

let result _ { text; _ } = Text.to_string text
let destruct_result _ text = { (default ()) with text = Text.of_string text }

let v =
  elt ~construct_state ~destruct_state ~default ~size ~render ~update
    ~destruct_result ~result ()
