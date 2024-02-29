open Common
module Ttf = Tsdl_ttf

type t = { buffer : Sdl.rw_ops; sizes : (int, Ttf.font) Hashtbl.t }

let load binstring =
  let& buffer = Tsdl.Sdl.rw_from_const_mem binstring in
  { buffer; sizes = Hashtbl.create 16 }

let get font size =
  match Hashtbl.find font.sizes size with
  | font -> font
  | exception Not_found ->
      (* Format.printf "load font in size %#i@." size ; *)
      let& ft = Ttf.open_font_rw font.buffer 0 size in
      Hashtbl.replace font.sizes size ft;
      ft

let draw font size text =
  lazy
    (let font = get font size in
     let& bmp =
       Ttf.render_text_solid font text
         (Tsdl.Sdl.Color.create ~r:0xFF ~g:0xFF ~b:0xFF ~a:0xFF)
     in
     Bitmap.of_texture bmp)
