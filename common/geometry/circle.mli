type t

val v : Point.t -> float -> t
val center : t -> Point.t
val radius : t -> float
val translate : t -> Vec.t -> t
