open Gamelle_common
open Geometry
open Brr_canvas
module C = C2d

let draw_clip ~io ctx f =
  let clip = io.clip in
  Option.iter
    (fun clip ->
      C.save ctx;
      ignore @@ Jv.call (C.to_jv ctx) "beginPath" [||];
      ignore
      @@ Jv.call (C.to_jv ctx) "rect"
           (Array.map Jv.of_float
              [|
                Box.x_left clip; Box.y_top clip; Box.width clip; Box.height clip;
              |]);

      ignore @@ Jv.call (C.to_jv ctx) "clip" [||])
    clip;
  let r = f () in
  if Option.is_some clip then C.restore ctx;
  r
