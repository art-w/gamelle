open Gamelle_backend
open Geometry
open Ui_backend

type ('state, 'params, 'r) elt =
  ui:t ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  'params ->
  'r

module type Widget = sig
  type params
  type state
  type return

  val size : ts:(string -> size2) -> params -> size2
  val render : io:io -> params -> state -> box2 -> unit
  val update : io:io -> params -> state -> box2 -> state
  val result : state -> return
  val v : (state, params, return) elt
end

type 'params inert_elt =
  ui:t ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?render:(io:io -> 'params -> box2 -> unit) ->
  'params ->
  unit

module type Inert_widget = sig
  type params

  val size : ts:(string -> size2) -> params -> size2
  val render : io:io -> params -> box2 -> unit
  val v : params inert_elt
end

type ('state, 'params, 'r) node =
  ui:t ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> children_size:size2 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  'params ->
  'r

let render_nothing ~io:_ _ = ()

let padding_v_elt =
  Leaf { id = None; size = padding_y; weight = 0.; renderer = render_nothing }

let padding_h_elt =
  Leaf { id = None; size = padding_x; weight = 0.; renderer = render_nothing }

let padding_elt ~dir = match dir with V -> padding_v_elt | H -> padding_h_elt

let rec insert e li =
  match li with
  | [] -> []
  | [ elt ] -> [ elt ]
  | elt :: elt' :: li -> elt :: e :: insert e (elt' :: li)

let insert_padding ~dir rs = insert (padding_elt ~dir) rs

let elt ~(construct_state : 'state -> state) ~destruct_state
    ~(default : 'params -> 'state)
    ~(size : ts:(string -> size2) -> 'params -> size2)
    ~(render : io:io -> 'params -> 'state -> box2 -> unit) ~update ~result :
    ('state, 'params, 'result) elt =
 fun ~ui ?id ?(size = size) ?(render = render) params ->
  let default = construct_state (default params) in
  let id = { _stack = Stack.get (); _hint = id } in
  let box = query_layout ~ui ~id in
  let size = size ~ts:(ui_text_size ~ui) params in
  let tbl : state tbl = (ui_state ~ui).state in
  let prev_state = destruct_state (find ~default tbl id) in
  let state = update ~io:ui.io params prev_state box in
  Hashtbl.replace tbl id (construct_state state);
  render_leaf ~ui ~id ~size ~weight:1. (render params state);
  result state

let inert_elt ~ui ~size ~weight ~render params =
  let size = size ~ts:(ui_text_size ~ui) params in
  render_leaf ~ui ~weight ~size (fun ~io box -> render ~io params box)

(* let scroll :(scroll_box_state,(size1 * (unit->'a)), 'a ) elt = fun  *)

let node ~construct_state ~destruct_state ~dir ~default ~size ~size_for_self
    ~children_offset ~render ~update ~result : ('state, 'params, 'r) node =
 fun ~ui ?id ?(size = size) ?(render = render) params ->
  let id = { _stack = Stack.get (); _hint = id } in
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.blue box;
  let tbl = (ui_state ~ui).state in
  let default = construct_state (default params) in
  let prev_state = destruct_state (find ~default tbl id) in
  let children_io = View.clipped_events true @@ View.clipped box ui.io in
  let old_renderers = ui.renderers and old_io = ui.io in
  ui.io <- children_io;
  ui.renderers <- [];
  let result = result params in
  let children = insert_padding ~dir ui.renderers in
  let children_size = total_size ~dir children in
  let state = update ~io:children_io ~children_size box prev_state params in
  ui.renderers <- old_renderers;
  ui.io <- old_io;
  let children_offset = children_offset state in
  let size = size ~ts:(ui_text_size ~ui) ~children_size params in
  Hashtbl.replace tbl id (construct_state state);
  render_node ~ui ~dir:V ~weight:1. ~children_offset ~children_io ~children ~id
    ~size ~size_for_self (render params state);
  result

let inert_node ~ui ?id ~render ~weight ~size_for_self ~children_offset ~dir f =
  let id = { _stack = Stack.get (); _hint = id } in
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
