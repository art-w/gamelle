open Gamelle_backend
open Geometry
open Ui_backend

type ('state, 'params, 'r) elt =
  t * string ->
  ?id:int ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?weight:float ->
  ?style:style ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  'params ->
  'r

type 'params inert_elt =
  t * string ->
  ?id:int ->
  ?style:style ->
  ?size:(ts:(string -> size2) -> 'params -> size2) ->
  ?render:(io:io -> 'params -> box2 -> unit) ->
  'params ->
  unit

type ('state, 'params, 'r) node =
  t * string ->
  ?id:int ->
  ?style:style ->
  ?size:(ts:(string -> size2) -> children_size:size2 -> 'params -> size2) ->
  ?weight:float ->
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
  val result : params -> state -> return
  val v : (state, params, return) elt
end

module type Inert_widget = sig
  type params

  val size : ts:(string -> size2) -> params -> size2
  val render : io:io -> params -> box2 -> unit
  val v : params inert_elt
end

let render_nothing ~io:_ _ = ()

let padding_v_elt =
  {
    id = None;
    size = padding_y;
    weight = 0.;
    style = default_style;
    renderer = render_nothing;
  }

let padding_h_elt =
  {
    id = None;
    size = padding_x;
    weight = 0.;
    style = default_style;
    renderer = render_nothing;
  }

let padding_elt ~dir = match dir with V -> padding_v_elt | H -> padding_h_elt

let rec insert e li =
  match li with
  | [] -> []
  | [ elt ] -> [ elt ]
  | elt :: elt' :: li -> elt :: e :: insert e (elt' :: li)

let insert_padding ~dir rs = insert (padding_elt ~dir) rs

let elt ~(construct_state : 'state -> state) ~destruct_state
    ~(default : 'params -> 'state) ?(weight = 1.)
    ~(size : ts:(string -> size2) -> 'params -> size2)
    ~(render : io:io -> 'params -> 'state -> box2 -> unit) ~update ~result () :
    ('state, 'params, 'result) elt =
 fun (ui, loc) ?id ?(size = size) ?(weight = weight) ?(style = default_style)
     ?(render = render) params ->
  let default = construct_state (default params) in
  let id = { loc_stack = loc :: ui.loc_stack; _hint = id } in
  let box = query_layout ~ui ~id in
  let size = size ~ts:(ui_text_size ~ui) params in
  let box = apply_style style box size in
  let tbl : state tbl = (ui_state ~ui).state in
  let prev_state = destruct_state (find ~default tbl id) in
  let state = update ~io:ui.io params prev_state box in
  Hashtbl.replace tbl id (construct_state state);
  render_leaf ~ui ~id ~style ~size ~weight (render params state);
  result params state

let inert_elt (ui, _loc) ~size ~weight ~render params =
  let size = size ~io:ui.io params in
  render_leaf ~ui ~weight ~size (fun ~io box -> render ~io params box)

let nest ~ui ~children_io ~weight ~dir children =
  node_renderer ~ui ~dir ~size:(total_size ~dir children) ~weight
    ~children_offset:V2.zero ~children_io ~size_for_self:Size2.zero ~children
    render_nothing

let node ~construct_state ~children_io ?(weight = 1.) ~destruct_state ~dir
    ~default ~size ~size_for_self ~children_offset ~render ~update ~result () :
    ('state, 'params, 'r) node =
 fun (ui, loc) ?id ?(style = default_style) ?(size = size) ?(weight = weight)
     ?(render = render) params ->
  let id = { loc_stack = loc :: ui.loc_stack; _hint = id } in
  let _ = style in
  let box = query_layout ~ui ~id in
  debug_box ~ui ~color:Color.blue box;
  let tbl = (ui_state ~ui).state in
  let default = construct_state (default params) in
  let prev_state = destruct_state (find ~default tbl id) in
  let children_io = children_io ~io:ui.io box in
  let old_renderers = ui.renderers and old_io = ui.io in
  ui.io <- children_io;
  ui.renderers <- [];
  let result = result params in
  let children =
    [
      padding_elt ~dir;
      nest ~ui ~children_io ~weight ~dir:(flip dir)
        [
          padding_elt ~dir:(flip dir);
          nest ~ui ~children_io ~weight ~dir (insert_padding ~dir ui.renderers);
          padding_elt ~dir:(flip dir);
        ];
      padding_elt ~dir;
    ]
  in
  let children_size = total_size ~dir children in
  let state = update ~io:children_io ~children_size box prev_state params in
  ui.renderers <- old_renderers;
  ui.io <- old_io;
  let children_offset = children_offset state in
  let size = size ~ts:(ui_text_size ~ui) ~children_size params in
  Hashtbl.replace tbl id (construct_state state);
  render_node ~ui ~dir ~weight ~children_offset ~children_io ~children ~id ~size
    ~size_for_self (render params state);
  result

let inert_node (ui, _loc) ~render ~weight ~size_for_self ~children_offset ~dir f
    =
  let old_renderers = ui.renderers in
  ui.renderers <- [];
  let result = f () in
  let children = insert_padding ~dir ui.renderers in
  let children_size = total_size ~dir children in
  ui.renderers <- old_renderers;
  let size = children_size in
  render_node ~ui ~dir ~weight ~children_offset ~children_io:ui.io ~children
    ~size ~size_for_self render;
  result
