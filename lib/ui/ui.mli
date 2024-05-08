module type S = Ui_intf.S

include
  S
    with type io := Gamelle_backend.io
     and type point := Gamelle_common.Geometry.Point.t
     and type box := Gamelle_common.Geometry.Box.t
     and type size := Gamelle_common.Geometry.Size.t
