open Gamelle
open Geometry

type id = int

type direction =
  | Vertical of { max_width : size1 }
  | Horizontal of { max_height : size1 }

type t = {
  mutable io : io;
  id : id;
  mutable pos : p2;
  mutable direction : direction;
  mutable render : unit -> unit;
  mutable debug_render : unit -> unit;
}

type 'a tbl = (id, 'a) Hashtbl.t

let new_tbl () = Hashtbl.create 16

type scroll_box_state = { size : size2; offset : float; grasped : bool }
(** size is the width or height depending on a vertical on horizontal layout. *)

type slider_state = { v : float; grasped : bool }
type layout = { box : box2; parent : id option; children : id list }

type state = {
  checkboxes : bool tbl;
  sliders : slider_state tbl;
  scroll_boxes : scroll_box_state tbl;
  layout : layout tbl;
}

let new_state () =
  {
    checkboxes = new_tbl ();
    sliders = new_tbl ();
    scroll_boxes = new_tbl ();
    layout = new_tbl ();
  }

let state : state tbl = Hashtbl.create 256
let ui_state ~ui = Hashtbl.find state ui.id

let find ~default tbl key =
  match Hashtbl.find_opt tbl key with
  | None ->
      Hashtbl.add tbl key default;
      default
  | Some v -> v

type ('state, 'r) elt =
  ui:t ->
  id:id ->
  ?size:('state -> size2) ->
  ?render:(io:io -> box2 -> 'state -> unit) ->
  ?update:(io:io -> box2 -> 'state -> 'state) ->
  ?result:('state -> 'r) ->
  unit ->
  'r

let padding = 6.
let padding_x = V2.v padding 0.
let padding_y = V2.v 0. padding
let padding_vec = V2.(padding_x + padding_y)
let fg = Color.Gruvbox.Light.fg
let bg = Color.Gruvbox.Light.bg
let bg' = Color.Gruvbox.Light.bg1
let highlight = Color.Gruvbox.Light.blue
let lowlight = Color.Gruvbox.Light.gray
let text_size ~ui = text_size ~io:ui.io

let debug_render ~ui f =
  let prev_f = ui.debug_render in
  ui.debug_render <-
    (fun () ->
      prev_f ();
      f ui.io)

let render ~ui f =
  let prev_f = ui.render in
  let io = ui.io in
  ui.render <-
    (fun () ->
      prev_f ();
      f ~io)

let debug_box ~ui ~color box =
  debug_render ~ui (fun io -> draw_rect ~io ~color box)

let allocate_area ~ui (size : size2) =
  let pos = ui.pos in
  match ui.direction with
  | Vertical { max_width } ->
      let box = Box.v V2.(pos + padding_y) size in
      debug_box ~ui ~color:Color.red box;
      let max_width = Float.max max_width (Box.w box) in
      let pos = P2.(v (x pos) (Box.maxy box)) in
      ui.pos <- pos;
      ui.direction <- Vertical { max_width };
      box
  | Horizontal { max_height } ->
      let box = Box.v V2.(pos + padding_x) size in
      debug_box ~ui ~color:Color.blue box;
      let max_height = Float.max max_height (Box.h box) in
      let pos = P2.(v (Box.maxx box) (y pos)) in
      ui.pos <- pos;
      ui.direction <- Horizontal { max_height };
      box

let horizontal ~ui f =
  match ui.direction with
  | Horizontal _ -> failwith "Call to horizontal when already horizontal !"
  | Vertical { max_width } ->
      let old_pos = ui.pos in
      ui.pos <- V2.(ui.pos + padding_y - padding_x);
      ui.direction <- Horizontal { max_height = 0. };
      let r = f () in
      let max_height =
        match ui.direction with
        | Horizontal { max_height } -> max_height
        | Vertical _ -> failwith "inconsistent directions"
      in
      let width = P2.(x ui.pos -. x old_pos) in
      ui.pos <- V2.(old_pos + v 0. max_height + padding_y);
      debug_box ~ui ~color:Color.green
        (Box.v_corners old_pos V2.(ui.pos + v width 0.));
      let max_width = Float.max max_width width in
      ui.direction <- Vertical { max_width };
      r

let vertical ~ui f =
  match ui.direction with
  | Vertical _ -> failwith "Call to vertical when already vertical !"
  | Horizontal { max_height } ->
      let old_pos = ui.pos in
      ui.pos <- V2.(ui.pos + padding_x - padding_y);
      ui.direction <- Vertical { max_width = 0. };
      let r = f () in
      let max_width =
        match ui.direction with
        | Vertical { max_width } -> max_width
        | Horizontal _ -> failwith "inconsistent directions"
      in
      let height = P2.(y ui.pos -. y old_pos) in
      ui.pos <- V2.(old_pos + v max_width 0. + padding_x);
      let max_height = Float.max max_height height in
      ui.direction <- Horizontal { max_height };
      r

let ui ?(debug = false) ~io ~id pos f =
  let ctx =
    {
      io;
      id;
      pos = V2.(pos + padding_x);
      direction = Vertical { max_width = 0. };
      render = Fun.id;
      debug_render = Fun.id;
    }
  in
  if not (Hashtbl.mem state id) then Hashtbl.add state id (new_state ());
  let r = f ctx in
  let end_corner =
    match ctx.direction with
    | Vertical { max_width } ->
        P2.(v (max_width +. (2. *. padding)) (y ctx.pos +. padding))
    | Horizontal { max_height = _ } ->
        (*P2.(v  (x ctx.pos +. padding) (max_height +. (2. *. padding)))*)
        failwith "Inconsistent directions at ui level"
  in

  let box = Box.v_corners pos end_corner in
  fill_rect ~io ~color:bg box;
  draw_rect ~io ~color:fg box;
  ctx.render ();
  if debug then ctx.debug_render ();
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
  render ~ui (fun ~io ->
      fill_rect ~io ~color:bg' box;
      draw_rect ~io ~color:fg box;
      draw_string ~io ~color:fg Font.default ~size text V2.(pos + padding_x));
  is_clicked ~ui box

let is_checked ~ui ~id box =
  let ui_state = ui_state ~ui in
  let is_clicked = is_clicked ~ui box in
  let state_b = find ~default:false ui_state.checkboxes id in
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
  render ~ui (fun ~io ->
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
  let { io; _ } = ui in
  let ui_state = ui_state ~ui in
  let { v; grasped } =
    find ~default:{ v = max; grasped = false } ui_state.sliders id
  in
  let grasped =
    if grasped then not (View.clip_events false ~io @@ Event.is_up `click_left)
    else Event.is_pressed ~io `click_left && Box.mem (Event.mouse_pos ~io) box
  in

  let v =
    if grasped then
      Float.max min @@ Float.min max
      @@ (V2.x (Event.mouse_pos ~io) -. Box.minx box)
         *. (max -. min) /. Box.w box
         +. min
    else v
  in
  Hashtbl.replace ui_state.sliders id { v; grasped };
  v

let slider ~ui ~id ~w ~min ~max =
  let height = 20. in
  let box = allocate_area ~ui (Size2.v (w +. (2. *. padding)) height) in
  let line = Box.v_mid (Box.mid box) (Size2.v w 4.) in
  let slider_val =
    slider_val ~ui ~id
      ~box:(Box.v_mid (Box.mid line) (Size2.v w height))
      ~min ~max
  in
  render ~ui (fun ~io ->
      fill_rect ~io ~color:lowlight line;
      let slider_pos = (slider_val -. min) *. Box.w box /. (max -. min) in
      fill_rect ~io ~color:highlight
        (Box.v (Box.o line) (Size2.v slider_pos 4.));
      fill_circle ~io ~color:highlight
        (Circle.v (P2.v (Box.minx line +. slider_pos) (Box.midy line)) 8.));
  slider_val

let label ~ui text =
  let size = 20 in
  let text_size = text_size ~ui Font.default ~size text in
  let pos = Box.o (allocate_area ~ui text_size) in
  render ~ui (fun ~io -> draw_string ~io ~color:fg Font.default ~size text pos)

let scroll_box_state ~ui ~id =
  find
    ~default:{ size = Size2.zero; offset = 0.; grasped = false }
    (ui_state ~ui).scroll_boxes id

let set_scroll_box_state ~ui ~id s =
  Hashtbl.replace (ui_state ~ui).scroll_boxes id s

let width ~ui =
  match ui.direction with
  | Vertical { max_width } -> max_width
  | _ -> failwith "TODO : implement horizontal scrollbars"

let scroll_box ~ui ~id ~size f =
  let { size = previous_size; offset; grasped } = scroll_box_state ~ui ~id in
  let scroll_bar_width = 10. in
  let box =
    allocate_area ~ui
      (Size2.v
         (Size2.w previous_size +. scroll_bar_width)
         size (*for now we assume vertical*))
  in
  let real_height = Size2.h previous_size in
  let start_pos = V2.(Box.o box - v 0. offset + padding_x) in
  ui.pos <- start_pos;
  let previous_io = ui.io in
  render ~ui (fun ~io -> draw_rect ~io ~color:fg box);
  ui.io <- previous_io |> View.clipped box |> View.clipped_events true;
  let r = f () in
  let scroll_bar_height = size *. size /. real_height in
  let scroll_rail_box =
    Box.v
      (P2.v (Box.maxx box -. scroll_bar_width) (Box.miny box))
      (Size2.v scroll_bar_width size)
  in
  let mouse_pos = Event.mouse_pos ~io:ui.io in
  let max_offset = real_height -. size in
  let grasped =
    if grasped then
      not (View.clip_events false ~io:ui.io @@ Event.is_up `click_left)
    else
      Event.is_down ~io:ui.io `click_left
      && Box.mem (Event.mouse_pos ~io:ui.io) scroll_rail_box
  in
  let offset =
    max 0. @@ min max_offset
    @@
    if grasped then
      max_offset
      *. (size /. (size -. scroll_bar_height))
      *. (P2.y mouse_pos -. Box.miny scroll_rail_box)
      /. size
      -. (size /. 2.)
    else if Event.is_pressed ~io:ui.io `wheel then
      let amount = Event.wheel_delta ~io:ui.io in
      offset +. amount
    else offset
  in
  ui.io <- previous_io;
  let new_real_height = P2.y ui.pos -. P2.y start_pos +. padding in

  let scroll_bar_box =
    Box.v
      (P2.v
         (Box.maxx box -. scroll_bar_width)
         ((offset *. size /. real_height) +. Box.miny box))
      (Size2.v scroll_bar_width scroll_bar_height)
  in
  render ~ui (fill_rect ~color:lowlight scroll_rail_box);
  render ~ui (fill_rect ~color:highlight scroll_bar_box);
  ui.pos <- P2.v (Box.minx box) (Box.maxy box);
  let size = Size2.v (width ~ui -. scroll_bar_width) new_real_height in
  set_scroll_box_state ~ui ~id { size; offset; grasped };
  r
