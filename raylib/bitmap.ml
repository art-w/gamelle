open Common
open Gamelle_common

let detect_format data =
  if String.length data >= 4 then
    let byte i = Char.code data.[i] in
    if byte 0 = 0x89 && byte 1 = 0x50 && byte 2 = 0x4E then ".png"
    else if byte 0 = 0xFF && byte 1 = 0xD8 then ".jpg"
    else if byte 0 = 0x47 && byte 1 = 0x49 && byte 2 = 0x46 then ".gif"
    else ".png"
  else ".png"

type s = {
  texture : Raylib.Texture.t;
  src_x : float;
  src_y : float;
  w : int;
  h : int;
}

type t = (io, s) Delayed.t

let size t = (t.w, t.h)

let of_image img =
  let w = Raylib.Image.width img in
  let h = Raylib.Image.height img in
  let texture = Raylib.load_texture_from_image img in
  Raylib.unload_image img;
  { texture; src_x = 0.; src_y = 0.; w; h }

let sub t x y w h =
  Delayed.make @@ fun ~io ->
  let t = Delayed.force ~io t in
  { t with src_x = t.src_x +. float x; src_y = t.src_y +. float y; w; h }

let load binstring =
  Delayed.make @@ fun ~io:_ ->
  let ext = detect_format binstring in
  let img =
    Raylib.load_image_from_memory ext binstring (String.length binstring)
  in
  of_image img

let free ~io t =
  let t = Delayed.force ~io t in
  Raylib.unload_texture t.texture
