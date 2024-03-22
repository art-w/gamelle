open Gg

type t

val circle : P2.t -> float -> t
val segment : P2.t -> P2.t -> t
val polygon : P2.t list -> t
val rect : Box2.t -> t

(* *)
val translate : V2.t -> t -> t
val rotate : ?center:P2.t -> angle:float -> t -> t

(* *)

val center : t -> P2.t
val signed_area : t -> float
val distance2 : P2.t -> t -> float
val mem : P2.t -> t -> bool
val intersects : t -> t -> bool
val intersections : t -> t -> P2.t list
val nearest_points : P2.t -> t -> (P2.t * V2.t) list

(* *)

val separation_axis : t -> t -> V2.t option
val contact_points : t -> t -> float * P2.t list

(* *)
module Make (S : Draw.S) : sig
  val draw : io:S.io -> ?color:Color.t -> t -> unit
  val fill : io:S.io -> ?color:Color.t -> t -> unit
end
