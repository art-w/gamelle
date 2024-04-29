open Draw_geometry
open Ui_backend
open Widget_builder

let size ~io text = Text.size ~io text
let render ~io text box = centered_text ~io ~color:fg text box

let v (ui, loc) ?(style = { Style.default with growth = 0. }) text =
  inert_elt (ui, loc) ~style ~size ~render text
