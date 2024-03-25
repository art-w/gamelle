open Gamelle_geometry
open Ui_backend
open Widget_builder


let v ~ui ?(weight = 1.) f =
  inert_node ~ui ~render:render_nothing ~weight ~size_for_self:Size2.zero
    ~children_offset:V2.zero ~dir:V f
