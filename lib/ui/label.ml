open Gamelle_backend
open Geometry
open Ui_backend
open Widget_builder

let size ~ts text = ts text

let render ~io text box =
  let pos = Box.o box in
  draw_string ~io ~color:fg Font.default ~size:font_size text pos

let v ~ui ?(weight = 0.) text =
  inert_elt ~ui ~size:size ~weight ~render:render text