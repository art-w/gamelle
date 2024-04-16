open Brr
open Brr_webaudio
open Gamelle_common
open Geometry
module Color = Color
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
include Draw
include Jsoo

(* type ctx = C.t *)

let prev_now = ref 0.0
let now = ref 0.0
let clock () = !now
let dt () = !now -. !prev_now

module Window = struct
  let size ~io =
    let canvas = io.backend.canvas in
    let w = Canvas.w canvas in
    let h = Canvas.h canvas in
    Size.v (float w) (float h)

  let set_size ~io s =
    let w = s |> Size.w |> int_of_float and h = s |> Size.h |> int_of_float in
    let canvas = io.backend.canvas in
    Canvas.set_w canvas w;
    Canvas.set_h canvas h

  let box ~io = Box.v Vec.zero (size ~io)
end

let run state update =
  let canvas =
    match Document.find_el_by_id G.document (Jstr.of_string "target") with
    | None -> failwith "missing 'target' canvas"
    | Some elt ->
        Events_js.attach ~target:(El.as_target elt);
        Canvas.of_el elt
  in
  let open Jsoo in
  Canvas.set_w canvas (640 * 2);
  Canvas.set_h canvas (480 * 2);

  let ctx = C.get_context canvas in
  let audio = Audio.Context.create () in

  let backend = { canvas; ctx; audio } in

  let rec animate state =
    let _ = G.request_animation_frame (loop state) in
    ()
  and loop state elapsed =
    let open Events_backend in
    prev_now := !now;
    now := elapsed /. 1000.0;
    Events_js.new_frame ();
    let io = { (make_io backend) with event = !Events_js.current } in
    fill_rect ~io ~color:Color.black (Window.box ~io);
    let state = update ~io state in
    Events_js.current := reset_wheel !Events_js.current;
    animate state
  in
  animate state
