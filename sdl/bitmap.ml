open Common

type s = { bmp : Sdl.texture; bmp_x : int; bmp_y : int; w : int; h : int }
type t = s Lazy.t

let size t = (t.w, t.h)

let of_texture bmp =
  let& bmp = Sdl.create_texture_from_surface (render ()) bmp in
  let& _, _, (w, h) = Sdl.query_texture bmp in
  { bmp; bmp_x = 0; bmp_y = 0; w; h }

let sub t x y w h =
  lazy
    (let t = Lazy.force t in
     { bmp = t.bmp; bmp_x = t.bmp_x + x; bmp_y = t.bmp_y + y; w; h })

let load_file filename =
  lazy
    (let& bmp = Tsdl_image.load filename in
     of_texture bmp)

let load binstring =
  lazy
    (let rw = Sdl_buffer.load binstring in
     let& bmp = Tsdl_image.load_rw (Sdl_buffer.get rw) true in
     let _ = Sys.opaque_identity rw in
     of_texture bmp)

let free (lazy t) = Sdl.destroy_texture t.bmp
