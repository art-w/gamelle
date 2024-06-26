open Common
open Gamelle_common
module Ttf = Tsdl_ttf.Ttf

type t = font

let set_font font io = { io with font }
let set_font_size font_size io = { io with font_size }

let load binstring =
  Delayed.make @@ fun ~io ->
  { buffer = Sdl_buffer.load ~io binstring; sizes = Hashtbl.create 16 }

let default : t = load Gamelle_common.Font.default

let get ~io font size =
  let font = Delayed.force ~io font in
  match Hashtbl.find font.sizes size with
  | font -> font
  | exception Not_found ->
      let& ft = Ttf.open_font_rw (Sdl_buffer.get font.buffer) 0 size in
      clean_io ~io (fun () ->
          Hashtbl.remove font.sizes size;
          Ttf.close_font ft);
      Hashtbl.replace font.sizes size ft;
      ft

let get ~io font size =
  let font, size = Common.get_font ~io font size in
  get ~io font size

let draw ?color ?font ?size text =
  Delayed.make @@ fun ~io ->
  let font = get ~io font size in
  let color = get_color ~io color in
  let r, g, b, a = Geometry.Color.to_srgbi color in
  let a = int_of_float (a *. 255.) in
  let& bmp =
    Ttf.render_utf8_blended font text (Tsdl.Sdl.Color.create ~r ~g ~b ~a)
  in
  Bitmap.of_texture ~io bmp

let text_size ?font ?size text =
  Delayed.make @@ fun ~io ->
  let font = get ~io font size in
  let& w, h = Ttf.size_utf8 font text in
  (* TODO: transform according to io.transform? *)
  Geometry.(Size.v (float w) (float h))

let default_size = Gamelle_common.Font.default_size
