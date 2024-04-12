type io = Gamelle_common.io

include Gamelle_backend
module Ui = Ui
include Gamelle_extras
module Shape = Shape
module Physics = Physics
module View = View
module Transform = Gamelle_common.Transform

include Gg
module P2 = Gamelle_common.P2
module Segment = Gamelle_common.Segment
module Circle = Gamelle_common.Circle
module Box = Gamelle_common.Box
module Color = Gamelle_common.Color
module Size2 = Gamelle_common.Size2

module Event = Gamelle_common.Io.Event