open Gamelle

type state = unit

let init = ()

let () =
  Gamelle.run init @@ fun ~io state ->
  if Input.is_pressed ~io `escape then raise Exit;
  Box.draw ~io @@ Box.v (Point.v 10.0 10.0) (Point.v 100.0 100.0);
  Text.draw ~io "Hello world!" ~at:Vec.zero;
  state
