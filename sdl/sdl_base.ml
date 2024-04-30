module Sdl = Tsdl.Sdl
module Ttf = Tsdl_ttf.Ttf

let force = function Error (`Msg m) -> failwith m | Ok x -> x
let ( let& ) x f = f (force x)
let ( let* ) = Result.bind
let int = int_of_float
