open Gg

type t = { start : p2; end_ : p2 }

let v start end_ = { start; end_ }
let start { start; _ } = start
let end_ { end_; _ } = end_
let to_tuple { start; end_ } = (start, end_)
let eps = 0.0001
let vector { start; end_ } = V2.(end_ - start)

let intersection { start = p1; end_ = p2 } { start = q1; end_ = q2 } =
  (* https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect *)
  let cross_product2 p q = V2.((x p *. y q) -. (y p *. x q)) in
  let r = V2.(p2 - p1) and s = V2.(q2 - q1) in
  let r_cross_s = cross_product2 r s in
  if Float.equal_tol ~eps r_cross_s 0. then
    (* colinear case. We do not check for intersection in that case *)
    None
  else
    let t = V2.(cross_product2 (q1 - p1) s /. r_cross_s) in
    let u = V2.(cross_product2 (q1 - p1) r /. r_cross_s) in
    if t >= 0. && t <= 1. && u >= 0. && u <= 1. then
      let inter = V2.(p1 + (t * r)) in
      Some inter
    else None

let intersect s s' = Option.is_some (intersection s s')
