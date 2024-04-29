open Gamelle_backend
open Draw_geometry

type t
type kind = Movable | Immovable

val make :
  ?mass:float ->
  ?inertia:float ->
  ?restitution:float ->
  ?kind:kind ->
  Shape.t ->
  t

val center : t -> Point.t
val add_velocity : Vec.t -> t -> t
val add_rot_velocity : float -> t -> t
val update : dt:float -> t -> t
val fix_collisions : t list -> t list
val draw : io:io -> ?color:color -> t -> unit
val fill : io:io -> ?color:color -> t -> unit
