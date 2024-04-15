open Gamelle_common
open Draw_geometry
open Ui_backend
open Widget_builder

type params = float

type state = {
  text : string;
  offset : float;
  cursor : int;
  focused : bool;
  arrow_key : (int * [ `left | `right ]) option;
  char_key : (int * char) option;
}

type return = string
type Ui_backend.state += Text_input of state

let default _ =
  {
    text = "";
    offset = 0.;
    cursor = 0;
    focused = false;
    arrow_key = None;
    char_key = None;
  }

let construct_state s = Text_input s

let destruct_state s =
  match s with Text_input b -> b | _ -> raise IdTypeMismatch

let size ~ts width =
  let text_size = Size.(v width (h (ts "a"))) in
  Vec.(text_size + (2. * padding_xy))

let text_length ~io text = Size.w (Text.size ~io ~size:font_size text)
let cursor_offset ~io text cursor = text_length ~io (String.sub text 0 cursor)

let render ~io _params
    { text; offset; cursor; focused; arrow_key = _; char_key = _ } box =
  Box.fill ~io ~color:bg' box;
  Box.draw ~io ~color:(if focused then highlight else fg) box;
  let io = View.clipped box io in
  let nsize = Vec.(Box.size box - (2. * padding_xy)) in
  let box = Box.(v_mid (mid box) nsize) in
  Text.draw ~io ~color:fg ~size:font_size text Vec.(Box.o box + v offset 0.);
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
    if i >= String.length text then i
    else
      let subtext = String.sub text 0 i in
      let w = text_length ~io subtext in
      if w > x then if w -. x < x -. prev_w then i else i - 1 else loop w (i + 1)
  in
  loop 0. 0

let slow_frames = 60
let slow_frequency = 20
let fast_frequency = 3

let update ~io _param { text; offset; cursor; focused; arrow_key; char_key } box
    =
  let nsize = Vec.(Box.size box - (2. * padding_xy)) in
  let box = Box.(v_mid (mid box) nsize) in
  let width = max (Box.w box) 0. in
  let arrow_key =
    Option.map (fun (frames, dir) -> (frames + 1, dir)) arrow_key
  in
  let char_key = Option.map (fun (n, c) -> (n + 1, c)) char_key in
  let arrow_key =
    if Event.is_down ~io `arrow_left then Some (0, `left)
    else if Event.is_down ~io `arrow_right then Some (0, `right)
    else if Event.is_up ~io `arrow_left || Event.is_up ~io `arrow_right then
      None
    else arrow_key
  in
  let module Chars = Event.Chars in
  let char_key =
    let char_down = Event.down_chars ~io |> Chars.choose_opt in
    match char_down with
    | Some c -> Some (0, c)
    | None -> (
        match char_key with
        | Some (_, c) when Chars.mem c (Event.up_chars ~io) -> None
        | _ -> char_key)
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
      (Int.min (String.length text)
         (match arrow_key with
         | None -> cursor
         | Some (0, dir) -> (
             cursor + match dir with `left -> -1 | `right -> 1)
         | Some (n, dir) when n < slow_frames && n mod slow_frequency = 0 -> (
             cursor + match dir with `left -> -1 | `right -> 1)
         | Some (n, dir) when n >= slow_frames && n mod fast_frequency = 0 -> (
             cursor + match dir with `left -> -1 | `right -> 1)
         | _ -> cursor))
  in
  let char_to_add =
    match char_key with
    | None -> None
    | Some (0, char) -> Some char
    | Some (n, char) when n < slow_frames && n mod slow_frequency = 0 ->
        Some char
    | Some (n, char) when n >= slow_frames && n mod fast_frequency = 0 ->
        Some char
    | _ -> None
  in
  let cursor, text =
    match char_to_add with
    | None -> (cursor, text)
    | Some char ->
        ( cursor + 1,
          String.sub text 0 cursor
          ^ String.of_seq (Seq.return char)
          ^ String.sub text cursor (String.length text - cursor) )
  in
  let cursor_pos = cursor_offset ~io text cursor +. offset in
  let offset =
    if cursor_pos < 0. then offset +. text_length ~io (String.sub text cursor 1)
    else if cursor_pos > width then
      let char_text = String.sub text (cursor - 1) 1 in
      let char_len = text_length ~io char_text in
      offset -. char_len
    else offset
  in
  { text; offset; cursor; focused; arrow_key; char_key }

let result _ { text; _ } = text

let v =
  elt ~construct_state ~destruct_state ~default ~size ~render ~update ~result ()
