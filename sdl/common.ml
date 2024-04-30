include Sdl_base
module Delayed = Gamelle_common.Delayed

type font_s = { buffer : Sdl_buffer.t; sizes : (int, Ttf.font) Hashtbl.t }

type io_backend = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  font : font;
  font_size : int;
}

and io = io_backend Gamelle_common.abstract_io
and font = (io, font_s) Delayed.t

let clock = Gamelle_common.clock
let dt = Gamelle_common.dt
let get_font ~io = function Some font -> font | None -> io.font
let get_font_size ~io = function Some size -> size | None -> io.font_size

let get_font ~io font_opt font_size =
  let io = io.Gamelle_common.backend in
  (get_font ~io font_opt, get_font_size ~io font_size)

let[@inline never] mutex_protect m f =
  let open Mutex in
  lock m;
  match f () with
  | x ->
      unlock m;
      x
  | exception e ->
      (* NOTE: [unlock] does not poll for asynchronous exceptions *)
      unlock m;
      raise e

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : io:io -> 'a -> 'a;
      clean : (unit -> unit) list;
    }
      -> run
