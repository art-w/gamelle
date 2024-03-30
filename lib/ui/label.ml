open Gamelle_backend
open Ui_backend
open Widget_builder

let size ~ts text = ts text

let render ~io text box =
  centered_text ~io ~color:fg Font.default ~size:font_size text box

let v (ui, loc) ?(style=default_style) ?(weight = 0.) text =
  inert_elt (ui, loc) ~style ~size ~weight ~render text

