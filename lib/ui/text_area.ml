open Draw_geometry
open Ui_backend
open Widget_builder

type params = { text : string; width : float option }

let size ~io { text; width } =
  Text.size_multiline ~io ?width ~size:font_size text

let render ~io { text; width } box =
  Text.draw_multiline ~io ~color:fg ?width ~size:font_size text (Box.o box)

let v (ui, loc) ?(style = default_style) ?width text =
  inert_elt (ui, loc) ~style ~size ~render { text; width }
