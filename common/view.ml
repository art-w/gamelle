type 'a abstract_io = {
  view : Transform.t;
  event : Events_backend.t;
  clip : Geometry.Box.t option;
  clip_events : bool;
  z_index : int;
  color : Color.t;
  window_size : (int * int) ref;
  clean : (unit -> unit) list ref;
  draws : (int * (unit -> unit)) list ref;
  backend : 'a;
}

let translate dxy io = { io with view = Transform.translate dxy io.view }
let scale factor io = { io with view = Transform.scale factor io.view }
let rotate angle io = { io with view = Transform.rotate angle io.view }
let clip clip io = { io with clip = Some clip }
let unclip io = { io with clip = None }
let clip_events b io = { io with clip_events = b }
let z_index z io = { io with z_index = z }
let color c io = { io with color = c }
