include Gg.Color
module Gruvbox = Gruvbox

let rgb ?(alpha = 1.0) r g b =
  let f x = float x /. 255.0 in
  v (f r) (f g) (f b) alpha
