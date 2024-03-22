module Sdl = Tsdl.Sdl
module Tsdl_image = Tsdl_image.Image
module Tsdl_ttf = Tsdl_ttf.Ttf
module Io = Gamelle_common.Io

let global_render : Sdl.renderer option ref = ref None
let set_render r = global_render := Some r
let render () = Option.get !global_render

(* let start_time = ref 0.0 *)
(* let now = ref 0.0 *)
(* let now_prev = ref 0.0 *)
(* let clock io = !now -. !start_time *)
(* let dt () = !now -. !now_prev *)
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

type io = Io.t

type run =
  | No_run : run
  | Run : {
      state : 'a;
      update : io:io -> 'a -> 'a;
      clean : (unit -> unit) list;
    }
      -> run
