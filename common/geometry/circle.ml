type t = { center : Point.t; radius : float }

let v center radius = { center; radius }
let center { center; _ } = center
let radius { radius; _ } = radius

let translate { center; radius } vec =
  { center = Point.translate center vec; radius }
