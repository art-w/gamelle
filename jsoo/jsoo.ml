module Canvas = Brr_canvas.Canvas
module C = Brr_canvas.C2d
open Brr_webaudio

type font = string Lazy.t

type io_backend = {
  canvas : Canvas.t;
  ctx : C.t;
  audio : Audio.Context.t;
  font : font;
  font_size : int;
}

type io = io_backend Gamelle_common.abstract_io
