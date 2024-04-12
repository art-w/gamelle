open Brr
open Globals
open Gamelle_common
open Geometry

type t = string Lazy.t

let uid = ref 0

let gen () =
  let u = !uid in
  uid := u + 1;
  u

let load binstring =
  lazy
    (let name = "GamelleFont" ^ string_of_int (gen ()) in
     let arr = Bitmap.tarray_of_string binstring in
     let fontface_class = Jv.get Jv.global "FontFace" in
     let ttf =
       Jv.new' fontface_class [| Jv.of_string name; Tarray.to_jv arr |]
     in
     let loading = Jv.call ttf "load" [||] in
     let _ =
       Jv.call loading "then"
         [|
           Jv.callback ~arity:1 (fun fontface ->
               let fonts = Jv.get (Document.to_jv G.document) "fonts" in
               let _res = Jv.call fonts "add" [| fontface |] in
               ());
           Jv.callback ~arity:1 (fun e -> Console.(log [ "font error:"; e ]));
         |]
     in
     name)

let default = load Gamelle_common.Font.default
let default_size = Gamelle_common.Font.default_size

let draw_at ~io (lazy font_name) ~size text (x, y) =
  C.set_font (render ())
    (Jstr.of_string (string_of_int size ^ "px " ^ font_name));
  let text = Jstr.of_string text in
  let metrics = C.measure_text (render ()) text in
  let ctx = render () in
  Clip.draw_clip ~io ctx (fun () ->
      C.fill_text ctx text ~x
        ~y:(y +. C.Text_metrics.font_bounding_box_ascent metrics))

let text_size (lazy font_name) ~size text =
  C.set_font (render ())
    (Jstr.of_string (string_of_int size ^ "px " ^ font_name));
  let text = Jstr.of_string text in
  let metrics = C.measure_text (render ()) text in
  let w = C.Text_metrics.width metrics in
  let h =
    C.Text_metrics.font_bounding_box_ascent metrics
    +. C.Text_metrics.font_bounding_box_descent metrics
  in
  Size.v w h

let draw ~color:_ _ _ _ = failwith "Font.draw"
