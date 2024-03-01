open Brr
open Common
module Bitmap = Bitmap
module Event = Event
module Font = Font
module Sound = Sound

(* type ctx = C.t *)

let prev_now = ref 0.0
let now = ref 0.0
let clock () = !now
let dt () = !now -. !prev_now

let window_size () =
  let canvas = Option.get !global_canvas in
  let w = Window.w canvas in
  let h = Window.h canvas in
  (float w, float h)

let draw bmp x y = Bitmap.draw ~ctx:(render ()) bmp ~x ~y
let fill_rect (x, y) (w, h) = C.fill_rect (render ()) ~x ~y ~w ~h
let draw_rect (x, y) (w, h) = C.stroke_rect (render ()) ~x ~y ~w ~h

let draw_line (x0, y0) (x1, y1) =
  let path = C.Path.create () in
  C.Path.move_to path ~x:x0 ~y:y0;
  C.Path.line_to path ~x:x1 ~y:y1;
  C.stroke (render ()) path

let set_color c =
  let r = (c lsr 24) land 0xFF in
  let g = (c lsr 16) land 0xFF in
  let b = (c lsr 8) land 0xFF in
  let a = c land 0xFF in
  let color =
    C.color (Jstr.of_string (Printf.sprintf "rgba(%i,%i,%i,%i)" r g b a))
  in
  C.set_fill_style (render ()) color;
  C.set_stroke_style (render ()) color

(* TODO *)
let draw_poly _ = ()
let fill_poly _ = ()

let show_cursor _ = ()
let tau = 8.0 *. atan 1.0

let draw_circle (x, y) radius =
  let path = C.Path.create () in
  C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
  C.stroke (render ()) path

let fill_circle _ _ = ()
let draw_thick_line ~stroke:_ _ _ = ()
let draw_string font ~size txt x y = Font.draw_at font ~size txt (x, y)

let run state ~update ~render =
  print_endline "hello!";
  let canvas =
    match Document.find_el_by_id G.document (Jstr.of_string "target") with
    | None -> failwith "missing 'target' canvas"
    | Some elt ->
        Event.attach ~target:(El.as_target elt);
        Window.of_el elt
  in

  let w = Window.w canvas in
  let h = Window.h canvas in
  global_canvas := Some canvas;

  Window.set_w canvas 640;
  Window.set_h canvas 480;

  let ctx = C.get_context canvas in
  global_ctx := Some ctx;

  let rec animate state =
    let _ = G.request_animation_frame (loop state) in
    ()
  and loop state elapsed =
    prev_now := !now;
    now := elapsed /. 1000.0;

    let state = update !Event.current state in
    C.set_fill_style ctx (C.color @@ Jstr.of_string "#000000");
    C.fill_rect ctx ~x:0.0 ~y:0.0 ~w:(float w) ~h:(float h);
    render state;
    animate state
  in
  animate state
