open Common
module Io = Gamelle_common.Io
module Delayed = Gamelle_common.Delayed
module Ttf = Tsdl_ttf

type s = { buffer : Sdl_buffer.t; sizes : (int, Ttf.font) Hashtbl.t }
type t = s Delayed.t

let load binstring =
  Delayed.make @@ fun ~io ->
  { buffer = Sdl_buffer.load ~io binstring; sizes = Hashtbl.create 16 }

let default = load Gamelle_common.Font.default

let get ~io font size =
  let font = Delayed.force ~io font in
  match Hashtbl.find font.sizes size with
  | font -> font
  | exception Not_found ->
      let& ft = Ttf.open_font_rw (Sdl_buffer.get font.buffer) 0 size in
      Io.clean ~io (fun () ->
          Hashtbl.remove font.sizes size;
          Ttf.close_font ft);
      Hashtbl.replace font.sizes size ft;
      ft

let draw ~color font size text =
  Delayed.make @@ fun ~io ->
  let font = get ~io font size in
  let r, g, b, a = Gg.Color.to_srgbi color in
  let a = int_of_float (a *. 255.) in
  let& bmp =
    Ttf.render_utf8_solid font text (Tsdl.Sdl.Color.create ~r ~g ~b ~a)
  in
  Bitmap.of_texture bmp
