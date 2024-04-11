open Gamelle_backend
open Gamelle_extras
open Ui_backend
open Widget_builder
open Gamelle_geometry

type params = { text : string; width : float option }

let size ~io { text; width } =
  text_area_size ~io ~font:Font.default ?width ~size:font_size text

let render ~io { text; width } box =
  draw_text ~io ~color:fg ~font:Font.default ?width ~size:font_size text
    (Box.o box)

let v (ui, loc) ?(style = default_style) ?(weight = 0.) ?width text =
  inert_elt (ui, loc) ~style ~size ~weight ~render { text; width }
