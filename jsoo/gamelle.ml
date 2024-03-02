open Brr
open Common
open Gg
module Color = Color
module Bitmap = Bitmap
module Event = Event
module Font = Font
module Sound = Sound
module View = Gamelle_common.View
include Draw

(* type ctx = C.t *)

let prev_now = ref 0.0
let now = ref 0.0
let clock () = !now
let dt () = !now -. !prev_now

let window_size () =
  let canvas = Option.get !global_canvas in
  let w = Window.w canvas in
  let h = Window.h canvas in
  Size2.v (float w) (float h)

let window_box () = Box2.v V2.zero (window_size ())

let run ?(on_exit = ignore) state ~update ~render =
  let _ = on_exit in
  (* wow *)
  let canvas =
    match Document.find_el_by_id G.document (Jstr.of_string "target") with
    | None -> failwith "missing 'target' canvas"
    | Some elt ->
        Event.attach ~target:(El.as_target elt);
        Window.of_el elt
  in

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
    let view = Gamelle_common.View.default in
    fill_rect ~view ~color:Color.black (window_box ());
    render ~view state;
    animate state
  in
  animate state
