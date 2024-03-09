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
  type t
  type 'a scene = view:t -> 'a

  val ( & ) : unit scene -> unit scene -> unit scene
  val translate : float * float -> 'a scene -> 'a scene
  val scale : float -> 'a scene -> 'a scene
  val rotate : float -> 'a scene -> 'a scene
  val default : t
  val translated : float * float -> t -> t
  val scaled : float -> t -> t
  val rotated : float -> t -> t
end

val clock : unit -> float
val dt : unit -> float
val draw : view:View.t -> Bitmap.t -> p2 -> unit
val draw_line : view:View.t -> color:Color.t -> Segment.t -> unit
val draw_rect : view:View.t -> color:Color.t -> box2 -> unit
val fill_rect : view:View.t -> color:Color.t -> box2 -> unit
val draw_poly : view:View.t -> color:Color.t -> p2 list -> unit
val fill_poly : view:View.t -> color:Color.t -> p2 list -> unit
val draw_circle : view:View.t -> color:Color.t -> Circle.t -> unit
val fill_circle : view:View.t -> color:Color.t -> Circle.t -> unit
val show_cursor : bool -> unit

val draw_string :
  view:View.t -> color:Color.t -> Font.t -> size:int -> string -> p2 -> unit

module Event : sig
  type t

  val mouse_pos : t -> float * float

  type key =
    | Escape
    | Control_left
    | Control_right
    | Arrow_left
    | Arrow_right
    | Arrow_up
    | Arrow_down
    | Char of char
    | Click_left
    | Click_right
    | Wheel_up
    | Wheel_down

  val is_pressed : t -> key -> bool
  val is_up : t -> key -> bool
  val is_down : t -> key -> bool
end

module Window : sig
  val set_size : int * int -> unit
  val size : unit -> size2
  val box : unit -> box2
end

val run :
  ?on_exit:('state -> unit) ->
  'state ->
  (view:View.t -> Event.t -> 'state -> 'state) ->
  unit

(* *)
module Shape : sig
  type t

  val circle : P2.t -> float -> t
  val segment : P2.t -> P2.t -> t
  val rect : Box2.t -> t
  val polygon : P2.t list -> t
  val draw : view:View.t -> color:Color.t -> t -> unit
  val fill : view:View.t -> color:Color.t -> t -> unit
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
  val draw : view:View.t -> t -> unit
end
