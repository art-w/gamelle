open Draw_geometry
open Ui_backend
open Widget_builder

let v (ui, loc) ?(style = Style.default) f =
  inert_node (ui, loc) ~style ~render:render_nothing ~size_for_self:Size.zero
    ~children_offset:Vec.zero ~dir:V f
