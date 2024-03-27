open Gamelle_backend
open Geometry
open Ui_backend
open Widget_builder

let size ~ts text = ts text

let render ~io text box =
  let pos = Box.o box in
  draw_string ~io ~color:fg ~at:pos text

let v (ui, loc) ?(weight = 0.) text =
  inert_elt (ui, loc) ~size ~weight ~render text
