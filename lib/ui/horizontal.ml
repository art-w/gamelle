open Draw_geometry
open Ui_backend
open Widget_builder

let v (ui, loc) ?(style = default_style) f =
  inert_node (ui, loc) ~style ~render:render_nothing ~size_for_self:Size.zero
    ~children_offset:Vec.zero ~dir:H f
