(**/**)

module Transform : module type of Transform
module Events_backend : module type of Events_backend
module Font : module type of Font
module Delayed : module type of Delayed
module Geometry : module type of Geometry
open Geometry

type 'a abstract_io = {
  view : Transform.t;
  event : Events_backend.t;
  centering_translation : vec;
  clip : box option;
  clip_events : bool;
  z_index : int;
  color : Color.t;
  window_size : (int * int) ref;
  clean : (unit -> unit) list ref;
  draws : (int * (unit -> unit)) list ref;
  backend : 'a;
}

val make_io : 'a -> 'a abstract_io
val clean_io : io:'a abstract_io -> (unit -> unit) -> unit
val clock : io:'a abstract_io -> float
val dt : io:'a abstract_io -> float
val z : io:'a abstract_io -> (io:'a abstract_io -> unit) -> unit
val get_color : io:'a abstract_io -> Color.t option -> Color.t
val finalize_frame : io:'a abstract_io -> unit
val max_sounds : int
