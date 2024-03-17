open Gamelle
open Geometry

type id = int

type t = {
  io : io;
  id : id;
  mutable pos : p2;
  mutable max_width : size1;
  mutable render : unit -> unit;
}

type 'a tbl = (id, 'a) Hashtbl.t
type state = { checkboxes : bool tbl; sliders : float tbl }

let new_state () =
  { checkboxes = Hashtbl.create 16; sliders = Hashtbl.create 16 }

let state : state tbl = Hashtbl.create 256
let padding = 6.
let padding_x = V2.v padding 0.
let padding_y = V2.v 0. padding
let padding_vec = V2.(padding_x + padding_y)
let fg = Color.Gruvbox.Light.fg
let bg = Color.Gruvbox.Light.bg
let bg' = Color.Gruvbox.Light.bg1
let highlight = Color.Gruvbox.Light.blue
let lowlight = Color.Gruvbox.Light.gray

let update_max_width ~ui candidate =
  ui.max_width <- Float.max ui.max_width candidate

let text_size ~ui = text_size ~io:ui.io

let render ~ui f =
  let prev_f = ui.render in
  ui.render <-
    (fun () ->
      prev_f ();
      f ui.io)

let allocate_area ~ui size =
  let pos = ui.pos in
  let box = Box.v V2.(pos + padding_y) size in
  update_max_width ~ui (Box.w box);
  let pos = P2.(v (x pos) (Box.maxy box)) in
  ui.pos <- pos;
  box

let ui ~io ~id pos f =
  let ctx =
    { io; id; pos = V2.(pos + padding_x); max_width = 0.; render = Fun.id }
  in
  if not (Hashtbl.mem state id) then Hashtbl.add state id (new_state ());
  let r = f ctx in
  let end_corner =
    P2.(v (ctx.max_width +. (2. *. padding)) (y ctx.pos +. padding))
  in
  let box = Box.v_corners pos end_corner in
  fill_rect ~io ~color:bg box;
  draw_rect ~io ~color:fg box;
  ctx.render ();
  (r, box)

let is_clicked ~ui box =
  let io = ui.io in
  Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

let button ~ui text =
  let size = 20 in
  let text_size = text_size ~ui Font.default ~size text in
  let box = allocate_area ~ui V2.(text_size + (2. * (padding_x + padding_y))) in
  let pos = Box.o box in
  let pos = V2.(pos + padding_y) in
  render ~ui (fun io ->
      fill_rect ~io ~color:bg' box;
      draw_rect ~io ~color:fg box;
      draw_string ~io ~color:fg Font.default ~size text V2.(pos + padding_x));
  let pos = P2.(v (x pos) (Box.maxy box)) in
  ui.pos <- pos;
  is_clicked ~ui box

let is_checked ~ui ~id box =
  let { id = id_ui; _ } = ui in
  let ui_state = Hashtbl.find state id_ui in
  let is_clicked = is_clicked ~ui box in
  let state_b =
    match Hashtbl.find_opt ui_state.checkboxes id with
    | None ->
        Hashtbl.add ui_state.checkboxes id false;
        false
    | Some b -> b
  in
  let r = if is_clicked then not state_b else state_b in
  Hashtbl.replace ui_state.checkboxes id r;
  r

let checkbox ~ui ~id text =
  let size = 20 in
  let text_size = text_size ~ui Font.default ~size text in
  let check'box'_size = Size2.h text_size in
  let box =
    allocate_area ~ui
      V2.(
        text_size
        + (2. * (padding_x + padding_y))
        + Size2.v (check'box'_size +. padding) 0.)
  in
  let pos = Box.o box in
  let pos = V2.(pos + padding_y) in
  let check'box' =
    Box.(
      v
        V2.(o box + padding_x + padding_y)
        Size2.(v check'box'_size check'box'_size))
  in
  let is_checked = is_checked ~ui ~id box in
  render ~ui (fun io ->
      fill_rect ~io ~color:bg' box;
      draw_rect ~io ~color:fg box;
      fill_rect ~io ~color:bg check'box';
      draw_rect ~io ~color:fg check'box';
      (if is_checked then
         let ticked'box' =
           Box.(
             v
               V2.(o check'box' + padding_x + padding_y)
               V2.(Box.size check'box' - (2. * padding_vec)))
         in
         fill_rect ~io ~color:highlight ticked'box');
      draw_string ~io ~color:fg Font.default ~size text
        V2.(pos + padding_x + v check'box'_size 0. + padding_x));
  is_checked

let slider_val ~ui ~id ~box ~min ~max =
  let { id = id_ui; io; _ } = ui in
  let ui_state = Hashtbl.find state id_ui in
  let is_clicked =
    Event.is_pressed ~io `click_left && Box.mem (Event.mouse_pos ~io) box
  in
  let state_n =
    match Hashtbl.find_opt ui_state.sliders id with None -> max | Some n -> n
  in
  let r =
    if is_clicked then
      ((V2.x (Event.mouse_pos ~io) -. Box.minx box) *. (max -. min) /. Box.w box)
      +. min
    else state_n
  in
  Hashtbl.replace ui_state.sliders id r;
  r

let slider ~ui ~id ~w ~min ~max =
  let height = 20. in
  let box = allocate_area ~ui (Size2.v (w +. (2. *. padding)) height) in
  let line = Box.v_mid (Box.mid box) (Size2.v w 4.) in
  let slider_val =
    slider_val ~ui ~id
      ~box:(Box.v_mid (Box.mid line) (Size2.v w height))
      ~min ~max
  in
  render ~ui (fun io ->
      fill_rect ~io ~color:lowlight line;
      let slider_pos = (slider_val -. min) *. Box.w box /. (max -. min) in
      fill_rect ~io ~color:highlight
        (Box.v (Box.o line) (Size2.v slider_pos 4.));
      fill_circle ~io ~color:highlight
        (Circle.v (P2.v (Box.minx line +. slider_pos) (Box.midy line)) 8.));
  update_max_width ~ui (Box.w box);
  slider_val

let label ~ui text =
  let size = 20 in
  let text_size = text_size ~ui Font.default ~size text in
  let pos = Box.o (allocate_area ~ui text_size) in
  render ~ui (fun io -> draw_string ~io ~color:fg Font.default ~size text pos)
