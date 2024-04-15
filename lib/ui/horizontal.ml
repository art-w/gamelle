open Draw_geometry
open Ui_backend
open Widget_builder

let v (ui, loc) ?(weight = 1.) f =
  inert_node (ui, loc) ~render:render_nothing ~weight ~size_for_self:Size.zero
    ~children_offset:Vec.zero ~dir:H f
