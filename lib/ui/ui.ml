open Draw_geometry
include Ui_backend
include Widget_builder

type cap = t * string

let button cap ?id ?weight ?style text = Button.v cap ?id ?weight ?style text

let slider ?id ?weight ?style ?(width = 50.) cap ~min ~max =
  let params = { w = width; min; max } in
  Slider.v cap ?id ?weight ?style params

let checkbox cap ?id ?weight ?style text =
  Checkbox.v cap ?id ?weight ?style text

let label cap ?weight ?style text = Label.v cap ?weight ?style text

let text_area cap ?weight ?style ?width text =
  Text_area.v cap ?weight ?width ?style text

let vscroll cap ?weight ?style ~height f =
  let params = { height; f } in
  Vscroll.v cap ?weight ?style params

let horizontal cap ?weight ?style f = Horizontal.v cap ?weight ?style f
let vertical cap ?weight ?style f = Vertical.v cap ?weight ?style f

let text_input cap ?id ?weight ?style float =
  Text_input.v cap ?id ?weight ?style float

let radio cap ?id ?weight ?style text = Radio.v cap ?id ?weight ?style text

module Customize = struct
  module Button = Button
  module Checkbox = Checkbox
  module Horizontal = Horizontal
  module Label = Label
  module Radio = Radio
  module Slider = Slider
  module Text_area = Text_area
  module Text_input = Text_input
  module Vertical = Vertical
  module Widget_builder = Widget_builder
end

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
  Box.fill ~io ~color:bg box;
  Box.draw ~io ~color:fg box;
  render ~ui box
    (node_renderer ~ui ~dir:H ~weight:1. ~children_offset:Vec.zero ~children
       ~children_io:io ~size ~size_for_self:Size.zero (fun ~io:_ _ -> ()));
  if debug then ui.debug_render ();
  (r, box)
