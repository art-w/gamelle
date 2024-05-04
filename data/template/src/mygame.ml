open Gamelle

let () =
  Gamelle.run 0.0 @@ fun ~io x ->
  Text.draw ~io "Hello Gamelle!" ~at:(Point.v x x);
  x +. 1.0
