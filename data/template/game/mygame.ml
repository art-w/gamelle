open Gamelle

let () =
  run @@ fun _ ->
    set_color 0x0000FFFF;
    draw_rect (10.0, 10.0) (100.0, 100.0)
