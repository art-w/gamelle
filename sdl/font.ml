open Common
module Ttf = Tsdl_ttf

type t = { buffer : Sdl_buffer.t; sizes : (int, Ttf.font) Hashtbl.t }

let load binstring =
  { buffer = Sdl_buffer.load binstring; sizes = Hashtbl.create 16 }

let default = load Gamelle_common.Font.default

let get font size =
  match Hashtbl.find font.sizes size with
  | font -> font
  | exception Not_found ->
      let& ft = Ttf.open_font_rw (Sdl_buffer.get font.buffer) 0 size in
      Hashtbl.replace font.sizes size ft;
      ft

let draw ~color font size text =
  lazy
    (let font = get font size in
     let r, g, b, a = Gg.Color.to_srgbi color in
     let a = int_of_float (a *. 255.) in
     let& bmp =
       Ttf.render_text_solid font text (Tsdl.Sdl.Color.create ~r ~g ~b ~a)
     in
     Bitmap.of_texture bmp)
