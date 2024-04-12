open Gamelle

type state = unit

let init = ()

let () =
  Gamelle.run init @@ fun ~io state ->
  if Event.is_pressed ~io `escape then raise Exit;
  let color = Color.v 1.0 1.0 0.0 1.0 in
  draw_rect ~io ~color (Box.v (Point.v 10.0 10.0) (Point.v 100.0 100.0));
  draw_string ~io ~color Font.default ~size:30 "Hello world!" Vec.zero;
  state
