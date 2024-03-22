open Gamelle
open Geometry

type id = int
type dir = V | H

type renderer =
  | Node of {
      id : id;
      dir : dir;
      children_pos : p2;
      children : renderer list;
      children_io : io;
      size : size2;
      renderer : io:io -> box2 -> unit;
    }
  | Leaf of { id : id; size : size2; renderer : io:io -> box2 -> unit }

type t = {
  mutable io : io;
  id : id;
  mutable renderers : renderer list;
  mutable debug_render : unit -> unit;
  mutable sizes : size2 list;
}

type 'a tbl = (id, 'a) Hashtbl.t

let new_tbl () = Hashtbl.create 16

type scroll_box_state = {
  size : size2;
  offset : float;
  grasped : bool;
  real_height : float;
}
(** size is the width or height depending on a vertical on horizontal layout. *)

type slider_state = { v : float; grasped : bool }

type state = {
  checkboxes : bool tbl;
  sliders : slider_state tbl;
  scroll_boxes : scroll_box_state tbl;
  buttons : bool tbl;
  layout : box2 tbl;
}

let new_state () =
  {
    checkboxes = new_tbl ();
    sliders = new_tbl ();
    scroll_boxes = new_tbl ();
    buttons = new_tbl ();
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

let register_layout ~ui ~id box = Hashtbl.replace (ui_state ~ui).layout id box
let query_layout ~ui ~id = find ~default:Box.zero (ui_state ~ui).layout id

type ('state, 'params, 'r) elt =
  ui:t ->
  id:id ->
  ?size:(io:io -> space_available:size1 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> 'params -> 'state -> box2 -> 'state) ->
  ?result:('state -> 'r) ->
  'params ->
  'r

type ('state, 'params, 'r) node =
  ui:t ->
  id:id ->
  ?size:
    (io:io -> space_available:size1 -> space_required:size1 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> children_size:size2 -> box2 -> 'state -> 'params -> 'state) ->
  ?result:('params -> 'r) ->
  'params ->
  'r

let font_size = 20
let padding = 6.
let padding_x = V2.v padding 0.
let padding_y = V2.v 0. padding
let padding_xy = V2.(padding_x + padding_y)
let fg = Color.Gruvbox.Light.fg
let bg = Color.Gruvbox.Light.bg
let bg' = Color.Gruvbox.Light.bg1
let highlight = Color.Gruvbox.Light.blue
let lowlight = Color.Gruvbox.Light.gray

let debug_render ~ui f =
  let prev_f = ui.debug_render in
  ui.debug_render <-
    (fun () ->
      prev_f ();
      f ui.io)

let render_leaf ~ui ~id ~size renderer =
  ui.renderers <- Leaf { id; size; renderer } :: ui.renderers

let render_node ~ui ~id ~dir ~children_pos ~children ~children_io ~size renderer
    =
  ui.renderers <-
    Node { id; dir; children_pos; children; children_io; size; renderer }
    :: ui.renderers

let debug_box ~ui ~color box =
  debug_render ~ui (fun io -> draw_rect ~io ~color box)

let allocate_area ~dir pos (size : size2) =
  match dir with
  | V ->
      let box = Box.v V2.(pos + padding_y) size in
      let pos = P2.(v (x pos) (Box.maxy box)) in
      (pos, box)
  | H ->
      let box = Box.v V2.(pos + padding_x) size in
      let pos = P2.(v (Box.maxx box) (y pos)) in
      (pos, box)

let register_size ~ui size = ui.sizes <- size :: ui.sizes

let total_size ~ui ~dir =
  let sizes = ui.sizes in
  let ws = List.map Size2.w sizes in
  let hs = List.map Size2.h sizes in
  let add s s' = s +. padding +. s' in
  let w_op, h_op = match dir with V -> (Float.max, add) | H -> (add, max) in
  Size2.v (List.fold_left w_op 0. ws) (List.fold_left h_op 0. hs)

let rec render ~ui ~dir pos = function
  | Leaf { id; size; renderer } ->
      let pos, box = allocate_area ~dir pos size in
      register_layout ~ui ~id box;
      debug_box ~ui ~color:Color.red box;
      renderer ~io:ui.io box;
      pos
  | Node { id; dir = dir'; children_pos; children_io; size; children; renderer }
    ->
      let children_pos =
        if dir <> dir' then
          match dir' with
          | V -> V2.(children_pos - padding_y)
          | H -> V2.(children_pos - padding_x)
        else children_pos
      in
      let old_io = ui.io in
      ui.io <- children_io;
      let _pos =
        children |> List.rev
        |> List.fold_left (render ~ui ~dir:dir') children_pos
      in
      ui.io <- old_io;
      let pos, box = allocate_area ~dir pos size in
      register_layout ~ui ~id box;
      renderer ~io:ui.io box;
      (*let pos =
          if dir <> dir' then
            match dir' with
            | V -> V2.(pos + padding_x)
            | H -> V2.(pos + padding_y)
          else pos
        in*)
      pos

let ui ?(debug = false) ~io ~id pos f =
  let ctx = { io; id; renderers = []; sizes = []; debug_render = Fun.id } in
  if not (Hashtbl.mem state id) then Hashtbl.add state id (new_state ());
  let r = f ctx in

  let end_corner = V2.(pos + total_size ~ui:ctx ~dir:V + (2. * padding_xy)) in
  let box = Box.v_corners pos end_corner in
  debug_box ~ui:ctx ~color:Color.green box;
  fill_rect ~io ~color:bg box;
  draw_rect ~io ~color:fg box;
  let _end_pos =
    ctx.renderers |> List.rev
    |> List.fold_left (render ~ui:ctx ~dir:V) V2.(pos + padding_xy)
  in
  if debug then ctx.debug_render ();
  (r, box)

let elt get_tbl ~(default : 'state) ~ui ~id
    ~(size : io:io -> space_available:size1 -> 'params -> size2)
    ~(render : io:io -> 'params -> 'state -> box2 -> unit) ~update ~result
    params =
  let box = query_layout ~ui ~id in
  let size = size ~io:ui.io ~space_available:0. params in
  register_size ~ui size;
  let tbl = get_tbl @@ ui_state ~ui in
  let prev_state = find ~default tbl id in
  let state = update ~io:ui.io params prev_state box in
  Hashtbl.replace tbl id state;
  render_leaf ~ui ~id ~size (render params state);
  result state

let is_clicked ~io box =
  Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

let button_size ~io ~space_available:_ text =
  let text_size = text_size ~io Font.default ~size:font_size text in
  V2.(text_size + (2. * (padding_x + padding_y)))

let button_render ~io text _is_clicked box =
  let pos = Box.o box in
  let pos = V2.(pos + padding_y) in
  fill_rect ~io ~color:bg' box;
  draw_rect ~io ~color:fg box;
  draw_string ~io ~color:fg Font.default ~size:font_size text
    V2.(pos + padding_x)

let button_update ~io _text _old_state box = is_clicked ~io box
let button_result b = b

let button : (bool, string, bool) elt =
 fun ~ui ~id ?(size = button_size) ?(render = button_render)
     ?(update = button_update) ?(result = button_result) text ->
  elt
    (fun s -> s.buttons)
    ~default:false ~ui ~id ~size ~render ~update ~result text

let checkbox_size ~io ~space_available:_ text =
  let text_size = text_size ~io Font.default ~size:font_size text in
  let check'box'_size = Size2.h text_size in
  V2.(
    text_size
    + (2. * (padding_x + padding_y))
    + Size2.v (check'box'_size +. padding) 0.)

let checkbox_render ~io text is_checked box =
  let pos = Box.o box in
  let pos = V2.(pos + padding_y) in
  let check'box'_size = Box.h box -. (2. *. padding) in
  let check'box' =
    Box.(
      v
        V2.(o box + padding_x + padding_y)
        Size2.(v check'box'_size check'box'_size))
  in
  fill_rect ~io ~color:bg' box;
  draw_rect ~io ~color:fg box;
  fill_rect ~io ~color:bg check'box';
  draw_rect ~io ~color:fg check'box';
  (if is_checked then
     let ticked'box' =
       Box.(
         v
           V2.(o check'box' + padding_xy)
           V2.(Box.size check'box' - (2. * padding_xy)))
     in
     fill_rect ~io ~color:highlight ticked'box');
  draw_string ~io ~color:fg Font.default ~size:font_size text
    V2.(pos + padding_x + v check'box'_size 0. + padding_x)

let checkbox_update ~io _text previous_is_checked box =
  let is_clicked = is_clicked ~io box in
  if is_clicked then not previous_is_checked else previous_is_checked

let checkbox_result is_clicked = is_clicked

let checkbox : (bool, string, bool) elt =
 fun ~ui ~id ?(size = checkbox_size) ?(render = checkbox_render)
     ?(update = checkbox_update) ?(result = checkbox_result) text ->
  elt
    (fun s -> s.checkboxes)
    ~default:false ~ui ~id ~size ~render ~update ~result text

type slider_params = { w : float; min : float; max : float }

let slider_size ~io:_ ~space_available:_ { w; min = _; max = _ } =
  let height = 20. in
  Size2.v (w +. (2. *. padding)) height

let slider_render ~io { w; min; max } state box =
  let slider_val = state.v in
  let line = Box.v_mid (Box.mid box) (Size2.v w 4.) in
  fill_rect ~io ~color:lowlight line;
  let slider_pos = (slider_val -. min) *. Box.w box /. (max -. min) in
  fill_rect ~io ~color:highlight (Box.v (Box.o line) (Size2.v slider_pos 4.));
  fill_circle ~io ~color:highlight
    (Circle.v (P2.v (Box.minx line +. slider_pos) (Box.midy line)) 8.)

let slider_update ~io { w = _; min; max } state box =
  let { v; grasped } = state in
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
  { v; grasped }

let slider_result state = state.v

let slider : (slider_state, slider_params, float) elt =
 fun ~ui ~id ?(size = slider_size) ?(render = slider_render)
     ?(update = slider_update) ?(result = slider_result) params ->
  let default = { v = params.max; grasped = false } in
  elt (fun s -> s.sliders) ~default ~ui ~id ~size ~render ~update ~result params

let label ~ui ~id text =
  let size = 20 in
  let text_size = text_size ~io:ui.io Font.default ~size text in
  register_size ~ui text_size;
  render_leaf ~ui ~id ~size:text_size (fun ~io box ->
      let pos = Box.o box in
      draw_string ~io ~color:fg Font.default ~size text pos)

(* let scroll :(scroll_box_state,(size1 * (unit->'a)), 'a ) elt = fun  *)

type 'a scroll_box_params = { height : float; f : unit -> 'a }

let scroll_bar_width = 10.

let scroll_box_size ~io:_ ~space_available ~space_required { height; f = _ } =
  let height = height in
  let width =
    Float.max space_available
      (space_required +. (2. *. padding) +. scroll_bar_width)
  in
  Size2.(v width height)

let scroll_box_render ~io { height; f = _ } state box =
  let { size = _; offset; grasped = _; real_height } = state in
  let scroll_rail_box =
    Box.v
      (P2.v (Box.maxx box -. scroll_bar_width) (Box.miny box))
      (Size2.v scroll_bar_width height)
  in
  let scroll_bar_height = height *. height /. real_height in

  let scroll_bar_box =
    Box.v
      (P2.v
         (Box.maxx box -. scroll_bar_width)
         ((offset *. height /. state.real_height) +. Box.miny box))
      (Size2.v scroll_bar_width scroll_bar_height)
  in
  draw_rect ~io ~color:fg box;
  fill_rect ~io ~color:lowlight scroll_rail_box;
  fill_rect ~io ~color:highlight scroll_bar_box

let scroll_box_update ~io ~children_size box state { height; f = _ } =
  let { size; offset; grasped; real_height = _ } = state in
  let real_height = Size2.h children_size in
  let scroll_rail_box =
    Box.v
      (P2.v (Box.maxx box -. scroll_bar_width) (Box.miny box))
      (Size2.v scroll_bar_width height)
  in
  let scroll_bar_height = height *. height /. real_height in
  let max_offset = real_height -. height in
  let mouse_pos = Event.mouse_pos ~io in
  let grasped =
    if grasped then not (View.clip_events false ~io @@ Event.is_up `click_left)
    else Event.is_down ~io `click_left && Box.mem mouse_pos scroll_rail_box
  in
  let offset =
    max 0. @@ min max_offset
    @@
    if grasped then
      max_offset
      *. (height /. (height -. scroll_bar_height))
      *. (P2.y mouse_pos -. Box.miny scroll_rail_box)
      /. height
      -. (height /. 2.)
    else if Event.is_pressed ~io `wheel then
      let amount = Event.wheel_delta ~io in
      offset +. amount
    else offset
  in
  { size; offset; grasped; real_height }

let scroll_box_result { height = _; f } = f ()

let scroll_box : type a. (scroll_box_state, a scroll_box_params, a) node =
 fun ~ui ~id ?(size = scroll_box_size) ?(render = scroll_box_render)
     ?(update = scroll_box_update) ?(result = scroll_box_result) params ->
  let default =
    { size = Size2.zero; offset = 0.; grasped = false; real_height = 0. }
  in
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.blue box;
  let ui_state = ui_state ~ui in
  let prev_state = find ~default ui_state.scroll_boxes id in
  let children_io = View.clipped_events true @@ View.clipped box ui.io in
  let old_sizes = ui.sizes
  and old_renderers = ui.renderers
  and old_io = ui.io in
  ui.io <- children_io;
  ui.sizes <- [];
  ui.renderers <- [];
  let result = result params in
  let children_size = total_size ~ui ~dir:V in
  let state = update ~io:children_io ~children_size box prev_state params in
  let children = ui.renderers in
  ui.sizes <- old_sizes;
  ui.renderers <- old_renderers;
  ui.io <- old_io;
  let children_pos = V2.(Box.o box - v 0. state.offset) in
  let space_available = Size2.w (total_size ~ui ~dir:V) in
  let space_required = Size2.w children_size in
  let size = size ~io:ui.io ~space_available ~space_required params in
  register_size ~ui size;
  Hashtbl.replace ui_state.scroll_boxes id state;
  render_node ~ui ~dir:V ~children_pos ~children_io ~children ~id ~size
    (render params state);
  result

let horizontal ~ui ~id f =
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.green box;
  let old_sizes = ui.sizes and old_renderers = ui.renderers in
  ui.sizes <- [];
  ui.renderers <- [];
  let result = f () in
  let children_size = total_size ~ui ~dir:H in
  let children = ui.renderers in
  ui.sizes <- old_sizes;
  ui.renderers <- old_renderers;
  let children_pos = Box.o box in
  let size = children_size in
  register_size ~ui size;
  render_node ~ui ~dir:H ~children_pos ~children_io:ui.io ~children ~id ~size
    (fun ~io:_ _ -> ());
  result

let vertical ~ui ~id f =
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.green box;
  let old_sizes = ui.sizes and old_renderers = ui.renderers in
  ui.sizes <- [];
  ui.renderers <- [];
  let result = f () in
  let children_size = total_size ~ui ~dir:V in
  let children = ui.renderers in
  ui.sizes <- old_sizes;
  ui.renderers <- old_renderers;
  let children_pos = Box.o box in
  let size = children_size in
  register_size ~ui size;
  render_node ~ui ~dir:V ~children_pos ~children_io:ui.io ~children ~id ~size
    (fun ~io:_ _ -> ());
  result
