open Draw_geometry
open Ui_backend
open Widget_builder

let size ~io text = Text.size ~io ~size:font_size text
let render ~io text box = centered_text ~io ~color:fg ~size:font_size text box

let v (ui, loc) ?(style = default_style) ?(weight = 0.) text =
  inert_elt (ui, loc) ~style ~size ~weight ~render text
