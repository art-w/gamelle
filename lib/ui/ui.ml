open Draw_geometry
open Ui_backend
open Widget_builder

module type S = Ui_intf.S

type t = Ui_backend.t

module Style = Style

type ui = t * string

let update_loc (ui, _loc) loc = (ui, loc)
let nest_loc = nest_loc
let button cap ?id ?init ?style text = Button.v cap ?id ?init ?style text

let slider ?id ?init ?style ?(width = 100.) cap ~min ~max =
  let params = Slider.{ w = width; min; max } in
  Slider.v cap ?id ?init ?style params

let int_slider ?id ?init ?style ?(width = 100.) cap ~min ~max =
  let params = Int_slider.{ w = width; min; max } in
  Int_slider.v cap ?id ?init ?style params

let checkbox cap ?id ?init ?style text = Checkbox.v cap ?id ?init ?style text
let label cap ?style text = Label.v cap ?style text
let text_area cap ?style ?width text = Text_area.v cap ?width ?style text

let vscroll cap ?style ~height f =
  let params = { height; f } in
  Vscroll.v cap ?style params

let horizontal cap ?style f = Horizontal.v cap ?style f
let vertical cap ?style f = Vertical.v cap ?style f

let text_input cap ?id ?init ?style float =
  Text_input.v cap ?id ?init ?style float

let radio cap ?id ?init ?style text = Radio.v cap ?id ?init ?style text

module Customize = struct
  module Button = Button
  module Checkbox = Checkbox
  module Horizontal = Horizontal
  module Label = Label
  module Radio = Radio
  module Slider = Slider
  module Int_slider = Int_slider
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
  let r = f (ui,"") in
  let dir = V in
  let children =
    [
      padding_h_elt;
      nest ~ui ~children_io:io
        ~style:{ Style.default with growth = 0. }
        ~dir:V
        [
          padding_v_elt;
          nest ~ui ~children_io:io
            ~style:{ Style.default with growth = 0. }
            ~dir:V
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
    (node_renderer ~ui ~dir:H ~style:Style.default ~children_offset:Vec.zero
       ~children ~children_io:io ~size ~size_for_self:Size.zero (fun ~io:_ _ ->
         ()));
  if debug then ui.debug_render ();
  (r, box)
