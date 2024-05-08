open Draw_geometry
open Ui_backend
open Widget_builder

type params = { text : string; width : float option }

let size ~io { text; width } = Text.size_multiline ~io ?width text

let render ~io { text; width } box =
  Text.draw_multiline ~io ~color:fg ?width ~at:(Box.top_left box) text

let v (ui, loc) ?(style = Style.default) ?width text =
  inert_elt (ui, loc) ~style ~size ~render { text; width }
