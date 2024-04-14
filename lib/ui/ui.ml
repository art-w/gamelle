open Gamelle_common.Geometry
open Gamelle_backend
include Ui_backend
include Widget_builder

type cap = t * string

let button = Button.v
let slider = Slider.v
let checkbox = Checkbox.v
let label = Label.v
let text_area = Text_area.v
let vscroll = Vscroll.v
let horizontal = Horizontal.v
let vertical = Vertical.v
let text_input = Text_input.v
let radio = Radio.v

let window ?(debug = false) ~io pos f =
  let id = pos in
  let ui = { io; id; renderers = []; debug_render = Fun.id; loc_stack = [] } in
  if not (Hashtbl.mem state id) then Hashtbl.add state id (new_state ());
  let state = ui_state ~ui in
   Hashtbl.reset state.used_ids;
  let r = f ui in
  let dir = V in
  let children =
    [
      padding_h_elt;
      nest ~ui ~children_io:io ~weight:0. ~dir:V
        [
          padding_v_elt;
          nest ~ui ~children_io:io ~weight:0. ~dir:V
            (insert_padding ~dir ui.renderers);
          padding_v_elt;
        ];
      padding_h_elt;
    ]
  in
  let size = total_size ~dir:H children in
  let end_corner = Vec.(pos + size) in
  let box = Box.v_corners pos end_corner in
  debug_box ~ui ~color:Color.green box;
  fill_rect ~io ~color:bg box;
  draw_rect ~io ~color:fg box;
  render ~ui box
    (node_renderer ~ui ~dir:H ~weight:1. ~children_offset:Vec.zero ~children
       ~children_io:io ~size ~size_for_self:Size.zero (fun ~io:_ _ -> ()));
  if debug then ui.debug_render ();
  (r, box)
