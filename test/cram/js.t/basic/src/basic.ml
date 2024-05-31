open Gamelle

let () =
  Gamelle.run () @@ fun ~io () ->
  Box.fill ~io ~color:Color.black (Box.v (Point.zero) (Size.v 300. 300.));
  Text.draw ~io "Hello Gamelle!" ~color:Color.red ~at:(Point.v 10. 10.)
