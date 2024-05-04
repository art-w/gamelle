module B = Gamelle_backend.Bitmap

type t = { w : int; h : int; bmp : B.t }

let width t = t.w
let height t = t.h
let dimensions t = (t.w, t.h)
let size t = Gamelle_common.Geometry.Size.v (float t.w) (float t.h)
let load ~w ~h bmp = { w; h; bmp = B.load bmp }
let sub ~x ~y ~w ~h t = { w; h; bmp = B.sub t.bmp x y w h }
let draw ~io ~at { bmp; _ } = Gamelle_common.z ~io (Gamelle_backend.draw bmp at)
