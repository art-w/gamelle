open Common

type s = {
  bmp : Sdl.texture;
  bmp_w : int;
  bmp_h : int;
  w : float;
  h : float;
  angle : float;
}

type t = s Lazy.t

let bmp_size t = (t.bmp_w, t.bmp_h)
let size t = (int t.w, int t.h)

let of_texture bmp =
  let& bmp = Sdl.create_texture_from_surface (render ()) bmp in
  let& _, _, (w, h) = Sdl.query_texture bmp in
  { bmp; bmp_w = w; bmp_h = h; w = float w; h = float h; angle = 0.0 }

let scale factor bmp =
  lazy
    (let bmp = Lazy.force bmp in
     { bmp with w = factor *. bmp.w; h = factor *. bmp.h })

let load_file filename =
  lazy
    (let& bmp = Tsdl_image.load filename in
     of_texture bmp)

let load binstring =
  lazy
    (let& buf = Tsdl.Sdl.rw_from_const_mem binstring in
     let& bmp = Tsdl_image.load_rw buf true in
     of_texture bmp)

let rotate angle (lazy t) = lazy { t with angle = t.angle +. angle }
