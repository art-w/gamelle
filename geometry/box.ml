open Gg
include Box2

let top box = Segment.v (tl_pt box) (tr_pt box)
let left box = Segment.v (tl_pt box) (bl_pt box)
let right box = Segment.v (tr_pt box) (br_pt box)
let bottom box = Segment.v (bl_pt box) (br_pt box)
let sides box = (top box, right box, bottom box, left box)
