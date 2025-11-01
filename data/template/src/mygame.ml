open Gamelle

let rec loop ~io x =
  Text.draw ~io "Hello Gamelle!" ~at:(Point.v x x);
  next_frame ~io;
  loop ~io (x +. 1.0)

let () = Gamelle.run (loop 0.0)
