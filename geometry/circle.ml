open Gg

type t = { center : p2; radius : size1 }

let v center radius = { center; radius }
let center { center; _ } = center
let radius { radius; _ } = radius
