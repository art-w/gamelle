open Brr
open Brr_webaudio
open Gamelle_common
open Geometry
module Text = Text
module Color = Color
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
include Draw
include Jsoo

let prev_now = ref 0.0
let now = ref 0.0
let clock = Gamelle_common.clock
let dt = Gamelle_common.dt

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

  let show_cursor ~io status =
    let canvas = io.backend.canvas in
    let el = Canvas.to_el canvas in
    if status then Brr.El.remove_inline_style Brr.El.Style.cursor el
    else Brr.El.set_inline_style Brr.El.Style.cursor (Jstr.of_string "none") el
end

let run ~canvas state update =
  let open Jsoo in
  Events_js.attach ~target:(El.as_target canvas);
  let canvas = Canvas.of_el canvas in
  Canvas.set_w canvas (640 * 2);
  Canvas.set_h canvas (480 * 2);

  let ctx = C.get_context canvas in
  let audio = Audio.Context.create () in

  let backend =
    { canvas; ctx; audio; font = Font.default; font_size = Font.default_size }
  in
  let clock_ref = ref 0 in

  let rec animate state =
    let _ = G.request_animation_frame (loop state) in
    ()
  and loop state elapsed =
    let open Events_backend in
    prev_now := !now;
    now := elapsed /. 1000.0;
    Events_js.new_frame ();
    let io =
      {
        (make_io backend) with
        event = { !Events_js.current with clock = !clock_ref };
      }
    in
    incr clock_ref;
    fill_rect ~io ~color:Color.black (Window.box ~io);
    let state = update ~io state in
    Gamelle_common.finalize_frame ~io;
    Events_js.current := reset_wheel !Events_js.current;
    animate state
  in
  animate state

let run state update =
  let canvas =
    match Document.find_el_by_id G.document (Jstr.of_string "target") with
    | None -> failwith "missing 'target' canvas"
    | Some elt -> elt
  in
  let started = ref false in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "focus"))
      (fun _ ->
        if !started then ()
        else (
          started := true;
          run ~canvas state update))
      (El.as_target canvas)
  in
  ()
