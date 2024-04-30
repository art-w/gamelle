open Brr
open Jsoo
open Gamelle_common
open Geometry

type t = font

let set_font font io = { io with font }
let set_font_size font_size io = { io with font_size }

let get_font ~io = function
  | Some (lazy font) -> font
  | None -> Lazy.force io.font

let get_font_size ~io = function Some size -> size | None -> io.font_size

let get_font ~io font size =
  let io = io.backend in
  (get_font ~io font, get_font_size ~io size)

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

let draw_at ~io ?color ?font ?size ~at text =
  let font_name, size = get_font ~io font size in
  Draw.set_color ~io color;
  let x, y = Gg.V2.to_tuple at in
  C.set_font io.backend.ctx
    (Jstr.of_string (string_of_int size ^ "px " ^ font_name));
  let text = Jstr.of_string text in
  let metrics = C.measure_text io.backend.ctx text in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      C.fill_text ctx text ~x
        ~y:(y +. C.Text_metrics.font_bounding_box_ascent metrics))

let text_size ~io ?font ?size text =
  let font_name, size = get_font ~io font size in
  C.set_font io.backend.ctx
    (Jstr.of_string (string_of_int size ^ "px " ^ font_name));
  let text = Jstr.of_string text in
  let metrics = C.measure_text io.backend.ctx text in
  let w = C.Text_metrics.width metrics in
  let h =
    C.Text_metrics.font_bounding_box_ascent metrics
    +. C.Text_metrics.font_bounding_box_descent metrics
  in
  Size.v w h
