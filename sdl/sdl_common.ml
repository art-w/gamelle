module Sdl = Tsdl.Sdl
module Tsdl_image = Tsdl_image.Image
module Tsdl_ttf = Tsdl_ttf.Ttf

let force = function Error (`Msg m) -> failwith m | Ok x -> x
let ( let& ) x f = f (force x)
