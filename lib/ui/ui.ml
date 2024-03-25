include Ui_backend
include Widget_builder

let button = Button.v
let slider = Slider.v
let checkbox = Checkbox.v
let label = Label.v
let vscroll = Vscroll.v
let horizontal = Horizontal.v
let vertical = Vertical.v

open Gamelle_backend
open Geometry

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
    (node_renderer ~ui:ctx ~dir:V ~weight:1. ~children_offset:V2.zero ~children
       ~children_io:io ~size ~size_for_self:Size2.zero (fun ~io:_ _ -> ()));
  if debug then ctx.debug_render ();
  (r, box)
