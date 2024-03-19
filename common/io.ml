open Gamelle_geometry

type t = {
  view : Transform.t;
  event : Event.t;
  clean : (unit -> unit) list ref;
  centering_translation : v2;
  clip : box2 option;
  clip_events : bool;
}

let make () =
  {
    view = Transform.default;
    event = Event.default;
    clean = ref [];
    centering_translation = V2.zero;
    clip = None;
    clip_events = false;
  }

let clean ~io fn = io.clean := fn :: !(io.clean)

(* *)

let translated dxy io = { io with view = Transform.translate dxy io.view }
let scaled factor io = { io with view = Transform.scale factor io.view }
let rotated angle io = { io with view = Transform.rotate angle io.view }
let clipped clip io = { io with clip = Some clip }
let unclipped io = { io with clip = None }
let clipped_events b io = { io with clip_events = b }

type 'a scene = io:t -> 'a

let ( & ) f g ~io =
  f ~io;
  g ~io

let translate dxy fn ~io = fn ~io:(translated dxy io)
let scale factor fn ~io = fn ~io:(scaled factor io)
let rotate angle fn ~io = fn ~io:(rotated angle io)
let project ~io p = Transform.project io.view p
let clip clip fn ~io = fn ~io:(clipped clip io)
let unclip fn ~io = fn ~io:(unclipped io)
let clip_events b fn ~io = fn ~io:(clipped_events b io)

type key = Event.key

let mouse_pos ~io = V2.(Event.mouse_pos io.event - io.centering_translation)

let handle_clip_events ~io b =
  if io.clip_events then
    match io.clip with
    | None -> b
    | Some clip -> if Box.mem (mouse_pos ~io) clip then b else false
  else b

let is_pressed ~io k = handle_clip_events ~io @@ Event.is_pressed io.event k
let is_up ~io k = handle_clip_events ~io @@ Event.is_up io.event k
let is_down ~io k = handle_clip_events ~io @@ Event.is_down io.event k
let wheel_delta ~io = Event.wheel_delta io.event
