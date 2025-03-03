include Gg.Color

let rgb ?(alpha = 1.0) r g b =
  let f x = float x /. 255.0 in
  v (f r) (f g) (f b) alpha

let hsl ?alpha h s l = Color.Hsl.(v ?alpha h s l |> to_gg)
let with_alpha alpha t = with_a t alpha
let yellow = v 1. 1. 0. 1.
let cyan = v 0. 1. 1. 1.
let magenta = v 1. 0. 1. 1.
let gray = v 0.5 0.5 0.5 1.
let purple = v 0.5 0. 1. 1.
let orange = v 1. 0.5 0. 1.
