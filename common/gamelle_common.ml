module Transform = Transform
module Events_backend = Events_backend
module Font = Font
module Delayed = Delayed
module Geometry = Geometry

let max_sounds = 256

open Geometry

type 'a abstract_io = {
  view : Transform.t;
  event : Events_backend.t;
  clean : (unit -> unit) list ref;
  centering_translation : vec;
  clip : box option;
  clip_events : bool;
  z_index : int;
  backend : 'a;
}

let make_io backend =
  {
    view = Transform.default;
    event = Events_backend.default;
    clean = ref [];
    centering_translation = Vec.zero;
    clip = None;
    clip_events = false;
    z_index = 0;
    backend;
  }

let clean_io ~io fn = io.clean := fn :: !(io.clean)
