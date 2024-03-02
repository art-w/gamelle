open Gg
open Gamelle

type state = unit

let update event () =
  if Event.is_pressed event Escape then raise Exit

let render ~view () =
  let color = Color.v 1.0 1.0 0.0 1.0 in
  draw_rect ~view ~color (Box2.v (P2.v 10.0 10.0) (P2.v 100.0 100.0))

let () =
  run () ~update ~render
