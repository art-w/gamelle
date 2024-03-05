open Gg
open Gamelle

type state = unit

let () =
  Gamelle.run () @@ fun ~view event () ->
  if Event.is_pressed event Escape then raise Exit;
  let color = Color.v 1.0 1.0 0.0 1.0 in
  draw_rect ~view ~color (Box2.v (P2.v 10.0 10.0) (P2.v 100.0 100.0));
  draw_string ~view ~color Font.default ~size:30 "Hello world!" V2.zero;
  ()
