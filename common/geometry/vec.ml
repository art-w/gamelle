include Gg.V2

let rotate_around ~angle:(cos, sin) ~center pt =
  let x, y = to_tuple @@ (pt - center) in
  let pt = v ((x *. cos) -. (y *. sin)) ((y *. cos) +. (x *. sin)) in
  pt + center
