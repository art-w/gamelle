open Gg
include P2

let translate p vec = Vec.(p + vec)
let lerp t a b = Vec.(a + (t * (b - a)))
