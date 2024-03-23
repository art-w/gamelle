open Gg
open Gamelle

type state = unit

let () =
  Gamelle.run () @@ fun ~io () ->
  if Event.is_pressed ~io `escape then raise Exit;
  let color = Color.v 1.0 1.0 0.0 1.0 in
  draw_rect ~io ~color (Box2.v (P2.v 10.0 10.0) (P2.v 100.0 100.0));
  draw_string ~io ~color "Hello world!" ~at:V2.zero;
  ()
