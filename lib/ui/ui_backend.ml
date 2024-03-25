open Gamelle_backend
open Geometry

type id = { _stack : Stack.t; _hint : int option }
type dir = V | H

let flip = function V -> H | H -> V

type renderer = {
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

type vscroll_state = {
  size : size2;
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
type state_layout = { state : state tbl; layout : box2 tbl }

let new_state () = { state = new_tbl (); layout = new_tbl () }
let state : (p2, state_layout) Hashtbl.t = Hashtbl.create 256
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
  ui.renderers <- { id; size; weight; renderer } :: ui.renderers
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

let total_size ~dir renderers =
  let sizes = List.map renderer_size renderers in
  let ws = List.map Size2.w sizes in
  let hs = List.map Size2.h sizes in
  let add s s' = s +. s' in
  let w_op, h_op = match dir with V -> (Float.max, add) | H -> (add, max) in
  Size2.v (List.fold_left w_op 0. ws) (List.fold_left h_op 0. hs)

let renderer_weight = function { weight; _ } -> weight

let rec render ~ui box = function
  | { id; size = _; weight = _; renderer } ->
      Option.iter (fun id -> register_layout ~ui ~id box) id;
      debug_box ~ui ~color:Color.red box;
      renderer ~io:ui.io box

and node_renderer ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
    ~children ~size_for_self renderer =
  let renderer ~io box =
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
        renderer ~io box
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
        renderer ~io box
  in
  { id; size; weight; renderer }

let render_node ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
    ~children ~size_for_self renderer =
  ui.renderers <-
    node_renderer ~ui ?id ~size ~weight ~dir ~children_offset ~children_io
      ~children ~size_for_self renderer
    :: ui.renderers

let io_text_size ~io = text_size ~io Font.default ~size:font_size
let ui_text_size ~ui = io_text_size ~io:ui.io

let is_clicked ~io box =
  Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box
