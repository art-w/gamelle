open Common

type s = { bmp : Sdl.texture; bmp_x : int; bmp_y : int; w : int; h : int }
type t = s Lazy.t

let size t = (t.w, t.h)

let of_texture bmp =
  let& bmp = Sdl.create_texture_from_surface (render ()) bmp in
  let& _, _, (w, h) = Sdl.query_texture bmp in
  { bmp; bmp_x = 0; bmp_y = 0; w; h }

let load_file filename =
  lazy
    (let& bmp = Tsdl_image.load filename in
     of_texture bmp)

let load binstring =
  lazy
    (let& buf = Tsdl.Sdl.rw_from_const_mem binstring in
     let& bmp = Tsdl_image.load_rw buf true in
     of_texture bmp)
