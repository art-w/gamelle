include Gg.Color

let rgb ?(alpha = 1.0) r g b =
  let f x = float x /. 255.0 in
  v (f r) (f g) (f b) alpha

let hsl ?(alpha = 1.0) h s l = Color.of_hsla h s l alpha
let with_alpha alpha t = with_a t alpha
