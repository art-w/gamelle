include Sdl_common
module Io = Gamelle_common.Io
module Delayed = Gamelle_common.Delayed

let global_render : Sdl.renderer option ref = ref None
let set_render r = global_render := Some r
let render () = Option.get !global_render
let ( let* ) = Result.bind
let int = int_of_float

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

type font_s = { buffer : Sdl_buffer.t; sizes : (int, Tsdl_ttf.font) Hashtbl.t }

type font = Font of (custom_io, font_s) Delayed.t
and custom_io = font

type io = font Io.t

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : io:io -> 'a -> 'a;
      clean : (unit -> unit) list;
    }
      -> run
