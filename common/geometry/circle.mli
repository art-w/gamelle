type t

val v : Point.t -> float -> t
val center : t -> Point.t
val radius : t -> float
val translate : t -> Vec.t -> t
val map_center : (Point.t -> Point.t) -> t -> t
val area : t -> float
val intersection : t -> t -> Point.t list
val intersects : t -> t -> bool
val mem : t -> Point.t -> bool
