type io

val run : 'state -> (io:io -> 'state -> 'state) -> unit

open Gamelle_geometry
module Geometry : module type of Gamelle_geometry
module Color : module type of Color

module Bitmap : sig
  type t

  val load : string -> t
  val sub : t -> int -> int -> int -> int -> t
end

module Font : sig
  type t

  val default : t
  val load : string -> t
  val draw : color:Color.t -> t -> int -> string -> Bitmap.t
end

module Sound : sig
  type sound

  val load : string -> sound
  val play : sound -> unit

  type music

  val load_music : string -> music
  val play_music : music -> unit
end

module View : sig
  type 'a scene = io:io -> 'a

  val ( & ) : unit scene -> unit scene -> unit scene
  val translate : float * float -> 'a scene -> 'a scene
  val scale : float -> 'a scene -> 'a scene
  val rotate : float -> 'a scene -> 'a scene
  val translated : float * float -> io -> io
  val scaled : float -> io -> io
  val rotated : float -> io -> io
end

val clock : unit -> float
val dt : unit -> float
val draw : io:io -> Bitmap.t -> p2 -> unit
val draw_line : io:io -> color:Color.t -> Segment.t -> unit
val draw_rect : io:io -> color:Color.t -> box2 -> unit
val fill_rect : io:io -> color:Color.t -> box2 -> unit
val draw_poly : io:io -> color:Color.t -> p2 list -> unit
val fill_poly : io:io -> color:Color.t -> p2 list -> unit
val draw_circle : io:io -> color:Color.t -> Circle.t -> unit
val fill_circle : io:io -> color:Color.t -> Circle.t -> unit
val show_cursor : bool -> unit

val draw_string :
  io:io -> color:Color.t -> Font.t -> size:int -> string -> p2 -> unit

module Event : sig
  val mouse_pos : io:io -> float * float

  type key =
    [ `quit
    | `escape
    | `control_left
    | `control_right
    | `arrow_left
    | `arrow_right
    | `arrow_up
    | `arrow_down
    | `char of char
    | `click_left
    | `click_right
    | `wheel_up
    | `wheel_down ]

  val is_pressed : io:io -> key -> bool
  val is_up : io:io -> key -> bool
  val is_down : io:io -> key -> bool
end

module Window : sig
  val set_size : int * int -> unit
  val size : unit -> size2
  val box : unit -> box2
end

(* *)
module Shape : sig
  type t

  val circle : P2.t -> float -> t
  val segment : P2.t -> P2.t -> t
  val rect : Box2.t -> t
  val polygon : P2.t list -> t
  val draw : io:io -> color:Color.t -> t -> unit
  val fill : io:io -> color:Color.t -> t -> unit
  val translate : V2.t -> t -> t
  val rotate : ?center:P2.t -> angle:float -> t -> t
  val center : t -> P2.t
  val distance2 : P2.t -> t -> float
  val mem : P2.t -> t -> bool
  val intersects : t -> t -> bool
  val intersections : t -> t -> P2.t list
  val nearest_points : P2.t -> t -> (P2.t * V2.t) list
end

module Physics : sig
  type t
  type kind = Movable | Immovable

  val make :
    ?mass:float ->
    ?inertia:float ->
    ?restitution:float ->
    ?kind:kind ->
    Shape.t ->
    t

  val center : t -> P2.t
  val add_velocity : V2.t -> t -> t
  val add_rot_velocity : float -> t -> t
  val update : dt:float -> t -> t
  val fix_collisions : t list -> t list
  val draw : io:io -> t -> unit
end
