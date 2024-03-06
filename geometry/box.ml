open Gg
include Box2

let top box = Segment.v (tl_pt box) (tr_pt box)
let left box = Segment.v (tl_pt box) (bl_pt box)
let right box = Segment.v (tr_pt box) (br_pt box)
let bottom box = Segment.v (bl_pt box) (br_pt box)
let sides box = (top box, right box, bottom box, left box)

let random_mem box =
  let x = Random.float (Box2.w box) +. Box2.ox box
  and y = Random.float (Box2.h box) +. Box2.oy box in
  P2.v x y