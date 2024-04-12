open Gg
module P2 = P2_

type t = { center : p2; radius : size1 }

let v center radius = { center; radius }
let center { center; _ } = center
let radius { radius; _ } = radius

let translate { center; radius } vec =
  { center = P2.translate center vec; radius }
