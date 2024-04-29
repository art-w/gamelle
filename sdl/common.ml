module Sdl = Tsdl.Sdl
module Delayed = Gamelle_common.Delayed

type io_backend = { window : Sdl.window; renderer : Sdl.renderer }
type io = io_backend Gamelle_common.abstract_io

let clock = Gamelle_common.clock
let dt = Gamelle_common.dt
let force = function Error (`Msg m) -> failwith m | Ok x -> x
let ( let& ) x f = f (force x)
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

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : io:io -> 'a -> 'a;
      clean : (unit -> unit) list;
    }
      -> run
