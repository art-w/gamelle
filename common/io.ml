open Gamelle_geometry

type t = {
  view : Transform.t;
  event : Event.t;
  clean : (unit -> unit) list ref;
  centering_translation : v2;
  clip : box2 option;
}

let make () =
  {
    view = Transform.default;
    event = Event.default;
    clean = ref [];
    centering_translation = V2.zero;
    clip = None;
  }

let clean ~io fn = io.clean := fn :: !(io.clean)

(* *)

let translated dxy io = { io with view = Transform.translate dxy io.view }
let scaled factor io = { io with view = Transform.scale factor io.view }
let rotated angle io = { io with view = Transform.rotate angle io.view }
let clipped clip io = { io with clip = Some clip }
let unclipped io = { io with clip = None }

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

type key = Event.key

let mouse_pos ~io = V2.(Event.mouse_pos io.event - io.centering_translation)
let is_pressed ~io k = Event.is_pressed io.event k
let is_up ~io k = Event.is_up io.event k
let is_down ~io k = Event.is_down io.event k

let wheel_delta ~io = Event.wheel_delta io.event