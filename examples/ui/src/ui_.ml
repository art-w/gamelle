open Gamelle
open Geometry

type id = { stack : Stack.t; hint : int option }
type dir = V | H

type renderer =
  | Node of {
      id : id option;
      weight : size1;
      dir : dir;
      children_offset : v2;
      children : renderer list;
      children_io : io;
      size : size2;
      size_for_self : size2;
      renderer : io:io -> box2 -> unit;
    }
  | Leaf of {
      id : id option;
      weight : size1;
      size : size2;
      renderer : io:io -> box2 -> unit;
    }

type t = {
  mutable io : io;
  id : p2;
  mutable renderers : renderer list;
  mutable debug_render : unit -> unit;
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

let state : (p2, state) Hashtbl.t = Hashtbl.create 256
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
  ?id:int ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> 'params -> 'state -> box2 -> 'state) ->
  ?result:('state -> 'r) ->
  'params ->
  'r

type ('state, 'params) inert_elt =
  ui:t ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?render:(io:io -> 'params -> box2 -> unit) ->
  'params ->
  unit

type ('state, 'params, 'r) node =
  ui:t ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> children_size:size2 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> children_size:size2 -> box2 -> 'state -> 'params -> 'state) ->
  ?result:('params -> 'r) ->
  'params ->
  'r

let font_size = 20
let padding = 4.
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

let render_leaf ~ui ?id ~weight ~size renderer =
  ui.renderers <- Leaf { id; size; weight; renderer } :: ui.renderers

let render_node ~ui ?id ~dir ~weight ~children_offset ~children ~children_io
    ~size ~size_for_self renderer =
  ui.renderers <-
    Node
      {
        id;
        dir;
        weight;
        children_offset;
        children;
        children_io;
        size;
        size_for_self;
        renderer;
      }
    :: ui.renderers

let debug_box ~ui ~color box =
  debug_render ~ui (fun io -> draw_rect ~io ~color box)

let renderer_size = function Leaf { size; _ } | Node { size; _ } -> size

let total_size ~dir renderers =
  let sizes = List.map renderer_size renderers in
  let ws = List.map Size2.w sizes in
  let hs = List.map Size2.h sizes in
  let add s s' = s +. s' in
  let w_op, h_op = match dir with V -> (Float.max, add) | H -> (add, max) in
  Size2.v (List.fold_left w_op 0. ws) (List.fold_left h_op 0. hs)

let renderer_weight = function
  | Leaf { weight; _ } | Node { weight; _ } -> weight

let padding_v_elt =
  Leaf
    { id = None; size = padding_y; weight = 0.; renderer = (fun ~io:_ _ -> ()) }

let padding_h_elt =
  Leaf
    { id = None; size = padding_x; weight = 0.; renderer = (fun ~io:_ _ -> ()) }

let padding_elt ~dir = match dir with V -> padding_v_elt | H -> padding_h_elt

let rec insert e li =
  match li with
  | [] -> []
  | [ elt ] -> [ elt ]
  | elt :: elt' :: li -> elt :: e :: insert e (elt' :: li)

let insert_padding ~dir rs = insert (padding_elt ~dir) rs

let rec render ~ui box = function
  | Leaf { id; size = _; weight = _; renderer } ->
      Option.iter (fun id -> register_layout ~ui ~id box) id;
      debug_box ~ui ~color:Color.red box;
      renderer ~io:ui.io box
  | Node
      {
        id;
        dir;
        children_offset;
        children_io;
        size = _;
        weight = _;
        children;
        renderer;
        size_for_self;
      } -> (
      match dir with
      | V ->
          debug_box ~ui ~color:Color.green box;
          let old_io = ui.io in
          ui.io <- children_io;
          let children_box = Box.(v (o box) V2.(size box - size_for_self)) in
          let children = List.rev children in
          let children_sizes = children |> List.map renderer_size in
          let min_space =
            children_sizes |> List.map Size2.h |> List.fold_left ( +. ) 0.
          in
          let leftovers = Float.max 0. (Box2.h children_box -. min_space) in
          let weights = children |> List.map renderer_weight in
          let total_weight = List.fold_left ( +. ) 0. weights in
          let coeffs = List.map (fun h -> h /. total_weight) weights in
          let children_sizes =
            List.map2
              (fun min_size coeff ->
                Size2.(
                  v (Box.w children_box) (h min_size +. (leftovers *. coeff))))
              children_sizes coeffs
          in
          let origin = V2.(children_offset + Box.o box) in
          let _taken =
            List.fold_left2
              (fun space_taken child child_size ->
                let box = Box.v V2.(origin + v 0. space_taken) child_size in
                let space_taken = space_taken +. Size2.h child_size in
                render ~ui box child;
                space_taken)
              0. children children_sizes
          in
          ui.io <- old_io;
          Option.iter (fun id -> register_layout ~ui ~id box) id;
          renderer ~io:ui.io box
      | H ->
          debug_box ~ui ~color:Color.blue box;
          let old_io = ui.io in
          ui.io <- children_io;
          (* compute pos here : compare weights *)
          let children = List.rev children in
          let children_sizes = children |> List.map renderer_size in
          let min_space =
            children_sizes |> List.map Size2.w |> List.fold_left ( +. ) 0.
          in
          let leftovers = Box2.w box -. min_space in
          let weights = children |> List.map renderer_weight in
          let total_weight = List.fold_left ( +. ) 0. weights in
          let coeffs = List.map (fun w -> w /. total_weight) weights in
          let children_sizes =
            List.map2
              (fun min_size coeff ->
                Size2.(v (w min_size +. (leftovers *. coeff)) (Box.h box)))
              children_sizes coeffs
          in
          let origin = V2.(children_offset + Box.o box) in
          let _pos =
            List.fold_left2
              (fun space_taken child child_size ->
                let box = Box.v V2.(origin + v space_taken 0.) child_size in
                let space_taken = space_taken +. Size2.w child_size in

                render ~ui box child;
                space_taken)
              0. children children_sizes
          in
          ui.io <- old_io;
          Option.iter (fun id -> register_layout ~ui ~id box) id;
          renderer ~io:ui.io box)

let ui ?(debug = false) ~io pos f =
  let id = pos in
  let ctx = { io; id; renderers = []; debug_render = Fun.id } in
  if not (Hashtbl.mem state id) then Hashtbl.add state id (new_state ());
  let r = f ctx in
  let children = insert_padding ~dir:V ctx.renderers in
  let size = total_size ~dir:V children in
  let end_corner = V2.(pos + size) in
  let box = Box.v_corners pos end_corner in
  debug_box ~ui:ctx ~color:Color.green box;
  fill_rect ~io ~color:bg box;
  draw_rect ~io ~color:fg box;
  render ~ui:ctx box
    (Node
       {
         id = None;
         dir = V;
         weight = 1.;
         children_offset = V2.zero;
         children;
         children_io = io;
         size;
         size_for_self = Size2.zero;
         renderer = (fun ~io:_ _ -> ());
       });
  if debug then ctx.debug_render ();
  (r, box)

let io_text_size ~io = text_size ~io Font.default ~size:font_size
let ui_text_size ~ui = io_text_size ~io:ui.io

let elt get_tbl ~(default : 'state) ~ui ~id
    ~(size : ts:(string -> size2) -> 'params -> size2)
    ~(render : io:io -> 'params -> 'state -> box2 -> unit) ~update ~result
    params =
  let id = { stack = Stack.get (); hint = id } in
  let box = query_layout ~ui ~id in
  let size = size ~ts:(ui_text_size ~ui) params in
  let tbl = get_tbl @@ ui_state ~ui in
  let prev_state = find ~default tbl id in
  let state = update ~io:ui.io params prev_state box in
  Hashtbl.replace tbl id state;
  render_leaf ~ui ~id ~size ~weight:1. (render params state);
  result state

let is_clicked ~io box =
  Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

let button_size ~ts text =
  let text_size = ts text in
  V2.(text_size + (2. * padding_xy))

let button_render ~io text _is_clicked box =
  let size = button_size ~ts:(io_text_size ~io) text in
  let pos = Box.o box in
  let box = Box.v pos size in
  fill_rect ~io ~color:bg' box;
  draw_rect ~io ~color:fg box;
  draw_string ~io ~color:fg Font.default ~size:font_size text
    V2.(Box.o box + padding_xy)

let button_update ~io _text _old_state box = is_clicked ~io box
let button_result b = b

let button : (bool, string, bool) elt =
 fun ~ui ?id ?(size = button_size) ?(render = button_render)
     ?(update = button_update) ?(result = button_result) text ->
  elt
    (fun s -> s.buttons)
    ~default:false ~ui ~id ~size ~render ~update ~result text

let checkbox_size ~ts text =
  let text_size = ts text in
  let check'box'_size = Size2.h text_size in
  V2.(text_size + (2. * padding_xy) + Size2.v (check'box'_size +. padding) 0.)

let checkbox_render ~io text is_checked box =
  let pos = Box.o box in
  let check'box'_size = Box.h box -. (2. *. padding) in
  let pos = V2.(pos + padding_y) in
  let check'box' =
    Box.(v V2.(o box + padding_xy) Size2.(v check'box'_size check'box'_size))
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
 fun ~ui ?id ?(size = checkbox_size) ?(render = checkbox_render)
     ?(update = checkbox_update) ?(result = checkbox_result) text ->
  elt
    (fun s -> s.checkboxes)
    ~default:false ~ui ~id ~size ~render ~update ~result text

type slider_params = { w : float; min : float; max : float }

let slider_size ~ts:_ { w; min = _; max = _ } =
  let height = 20. in
  Size2.v (w +. (2. *. padding)) height

let slider_render ~io { w = _; min; max } state box =
  let w = Box.w box in
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
 fun ~ui ?id ?(size = slider_size) ?(render = slider_render)
     ?(update = slider_update) ?(result = slider_result) params ->
  let default = { v = params.max; grasped = false } in
  elt (fun s -> s.sliders) ~default ~ui ~id ~size ~render ~update ~result params

let render_nothing ~ui:_ _ = ()

let inert_elt ~ui ~size ~weight ~render params =
  let size = size ~ts:(ui_text_size ~ui) params in
  render_leaf ~ui ~weight ~size (fun ~io box -> render ~io  params box)

let label_size ~ts text = ts text

let label_render ~io text box =
  let pos = Box.o box in
  draw_string ~io ~color:fg Font.default ~size:font_size text pos

let label ~ui ?(weight = 0.) text =
  inert_elt ~ui ~size:label_size ~weight ~render:label_render text

(* let scroll :(scroll_box_state,(size1 * (unit->'a)), 'a ) elt = fun  *)

type 'a scroll_box_params = { height : float; f : unit -> 'a }

let scroll_bar_width = 10.

let scroll_box_size ~ts:_ ~children_size { height; f = _ } =
  let height = height in
  let width =
    (* Float.max space_available *)
    Size2.w children_size +. scroll_bar_width
  in
  Size2.(v width height)

let scroll_box_render ~io { height = _; f = _ } state box =
  let height = Box.h box in
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

let scroll_box_update ~io ~children_size box state { height = _; f = _ } =
  let { size; offset; grasped; real_height = _ } = state in
  let height = Box.h box in
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
 fun ~ui ?id ?(size = scroll_box_size) ?(render = scroll_box_render)
     ?(update = scroll_box_update) ?(result = scroll_box_result) params ->
  let default =
    { size = Size2.zero; offset = 0.; grasped = false; real_height = 0. }
  in
  let id = { stack = Stack.get (); hint = id } in
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.blue box;
  let ui_state = ui_state ~ui in
  let prev_state = find ~default ui_state.scroll_boxes id in
  let children_io = View.clipped_events true @@ View.clipped box ui.io in
  let old_renderers = ui.renderers and old_io = ui.io in
  ui.io <- children_io;
  ui.renderers <- [];
  let result = result params in
  let children = insert_padding ~dir:V ui.renderers in
  let children_size = total_size ~dir:V children in
  let state = update ~io:children_io ~children_size box prev_state params in
  ui.renderers <- old_renderers;
  ui.io <- old_io;
  let children_offset = V2.(zero - v 0. state.offset) in
  let size = size ~ts:(ui_text_size ~ui) ~children_size params in
  Hashtbl.replace ui_state.scroll_boxes id state;
  render_node ~ui ~dir:V ~weight:1. ~children_offset ~children_io ~children ~id
    ~size
    ~size_for_self:Size2.(v scroll_bar_width 0.)
    (render params state);
  result

let inert_node ~ui ?id ~render ~weight ~size_for_self ~children_offset ~dir f =
  let id = { stack = Stack.get (); hint = id } in
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.green box;
  let old_renderers = ui.renderers in
  ui.renderers <- [];
  let result = f () in
  let children = insert_padding ~dir ui.renderers in
  let children_size = total_size ~dir children in
  ui.renderers <- old_renderers;
  let size = children_size in
  render_node ~ui ~dir ~weight ~children_offset ~children_io:ui.io ~children ~id
    ~size ~size_for_self render;
  result

let horizontal ~ui ?id ?(weight = 1.) f =
  inert_node ~ui ?id
    ~render:(fun ~io:_ _ -> ())
    ~weight ~size_for_self:Size2.zero ~children_offset:V2.zero ~dir:H f

let vertical ~ui ?id ?(weight = 1.) f =
  inert_node ~ui ?id
    ~render:(fun ~io:_ _ -> ())
    ~weight ~size_for_self:Size2.zero ~children_offset:V2.zero ~dir:V f
