open Gamelle

type state = unit

let update _event () = ()

let render () =
  set_color 0x0000FFFF;
  draw_rect (10.0, 10.0) (100.0, 100.0)

let () =
  run () ~update ~render
