open Gamelle_common
open Geometry
open Gamelle_backend

type id = { loc_stack : string list; _hint : int option }
type dir = V | H
type alignment = Start | End | Center | Fill
type style = { vertical : alignment; horizontal : alignment }

let default_style = { vertical = Center; horizontal = Fill }

let apply_style style box size =
  let h =
    match style.vertical with
    | Center | Start | End -> Size.h size
    | Fill -> Box.h box
  in
  let w =
    match style.horizontal with
    | Center | Start | End -> Size.w size
    | Fill -> Box.w box
  in
  let x =
    match style.horizontal with
    | Start | Fill -> Box.minx box
    | End -> Box.maxx box -. w
    | Center -> Box.midx box -. (w /. 2.)
  in
  let y =
    match style.vertical with
    | Start | Fill -> Box.miny box
    | End -> Box.maxy box -. h
    | Center -> Box.midy box -. (h /. 2.)
  in
  Box.v (Point.v x y) (Size.v w h)

let flip = function V -> H | H -> V

type renderer = {
  id : id option;
  weight : float;
  size : size;
  style : style;
  renderer : io:io -> box -> unit;
}

type t = {
  mutable io : io;
  id : point;
  mutable renderers : renderer list;
  mutable debug_render : unit -> unit;
  mutable loc_stack : string list;
}

type 'a tbl = (id, 'a) Hashtbl.t

let new_tbl () = Hashtbl.create 16

type vscroll_state = {
  size : size;
  offset : float;
  grasped : bool;
  real_height : float;
}
(** size is the width or height depending on a vertical on horizontal layout. *)

type slider_state = { v : float; grasped : bool }
type slider_params = { w : float; min : float; max : float }
type 'a vscroll_params = { height : float; f : unit -> 'a }

exception IdTypeMismatch

type state = ..
type state_layout = { state : state tbl; layout : box tbl }

let new_state () = { state = new_tbl (); layout = new_tbl () }
let state : (point, state_layout) Hashtbl.t = Hashtbl.create 256
let ui_state ~ui = Hashtbl.find state ui.id

let find ~default tbl key =
  match Hashtbl.find_opt tbl key with
  | None ->
      Hashtbl.add tbl key default;
      default
  | Some v -> v

let register_layout ~ui ~id box = Hashtbl.replace (ui_state ~ui).layout id box
let query_layout ~ui ~id = find ~default:Box.zero (ui_state ~ui).layout id
let font_size = 20
let padding = 4.
let padding_x = Vec.v padding 0.
let padding_y = Vec.v 0. padding
let padding_xy = Vec.(padding_x + padding_y)
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

let push_renderer ~ui renderer = ui.renderers <- renderer :: ui.renderers

let render_leaf ~ui ?id ~weight ~style ~size renderer =
  push_renderer ~ui { id; style; size; weight; renderer }
(*
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
    :: ui.renderers *)

let debug_box ~ui ~color box =
  debug_render ~ui (fun io -> draw_rect ~io ~color box)

let renderer_size ({ size; _ } : renderer) = size

let aggregate_sizes ~dir sizes =
  let ws = List.map Size.w sizes in
  let hs = List.map Size.h sizes in
  let add s s' = s +. s' in
  let w_op, h_op = match dir with V -> (Float.max, add) | H -> (add, max) in
  Size.v (List.fold_left w_op 0. ws) (List.fold_left h_op 0. hs)

let total_size ~dir renderers =
  aggregate_sizes ~dir (List.map renderer_size renderers)

let renderer_weight = function { weight; _ } -> weight
let size_dir ~dir = match dir with V -> Size.h | H -> Size.w
let box_size_dir ~dir = match dir with V -> Box.h | H -> Box.w
let basis_dir ~dir = match dir with V -> Vec.v 0. 1. | H -> Vec.v 1. 0.

let layout_boxes ~dir box weights sizes =
  let min_space =
    sizes |> List.map (size_dir ~dir) |> List.fold_left ( +. ) 0.
  in
  let leftovers = Float.max 0. (box_size_dir ~dir box -. min_space) in
  let total_weight = List.fold_left ( +. ) 0. weights in
  let coeffs = List.map (fun l -> l /. total_weight) weights in
  let children_sizes =
    if total_weight = 0. then
      List.map
        (fun min_size ->
          match dir with
          | V -> Size.(v (Box.w box) (h min_size))
          | H -> Size.(v (w min_size) (Box.h box)))
        sizes
    else
      List.map2
        (fun min_size coeff ->
          match dir with
          | V -> Size.(v (Box.w box) (h min_size +. (leftovers *. coeff)))
          | H -> Size.(v (w min_size +. (leftovers *. coeff)) (Box.h box)))
        sizes coeffs
  in
  let origin = Box.o box in
  let _taken, boxes =
    List.fold_left_map
      (fun space_taken child_size ->
        let box =
          Box.v Vec.(origin + (space_taken * basis_dir ~dir)) child_size
        in
        let space_taken = space_taken +. size_dir ~dir child_size in
        (space_taken, box))
      0. children_sizes
  in
  boxes

let rec render ~ui box = function
  | { id; size; style; weight = _; renderer } ->
      Option.iter (fun id -> register_layout ~ui ~id box) id;
      debug_box ~ui ~color:Color.red box;
      let box = apply_style style box size in
      renderer ~io:ui.io box

and node_renderer ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
    ~children ~size_for_self renderer =
  let renderer ~io box =
    debug_box ~ui ~color:Color.green box;
    let old_io = ui.io in
    ui.io <- children_io;
    let children_box =
      Box.(v Vec.(o box + children_offset) Vec.(size box - size_for_self))
    in
    let children = List.rev children in
    let children_sizes = children |> List.map renderer_size in
    let weights = children |> List.map renderer_weight in
    let boxes = layout_boxes ~dir children_box weights children_sizes in
    List.iter2 (fun box child -> render ~ui box child) boxes children;
    ui.io <- old_io;
    Option.iter (fun id -> register_layout ~ui ~id box) id;
    renderer ~io box
  in
  { id; size; weight; style = default_style; renderer }

let render_node ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
    ~children ~size_for_self renderer =
  push_renderer ~ui
    (node_renderer ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
       ~children ~size_for_self renderer)

let io_text_size ~io = text_size ~io ~size:font_size
let ui_text_size ~ui = io_text_size ~io:ui.io

let clicked_outside ~io box =
  Event.is_down ~io `click_left && (not @@ Box.mem (Event.mouse_pos ~io) box)

let is_clicked ~io box =
  Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

let centered_text ~io ~color ?font ?size text box =
  let text_size = text_size ~io ?font ?size text in
  let pos = Box.(o (v_mid (mid box) text_size)) in
  draw_string ~io ~color ?font ?size text pos

let nest_loc (ui, loc) f =
  ui.loc_stack <- loc :: ui.loc_stack;
  let r = f () in
  ui.loc_stack <- List.tl ui.loc_stack;
  r
