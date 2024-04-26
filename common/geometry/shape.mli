type t = Segment of Segment.t | Circle of Circle.t | Polygon of Polygon.t

val circle : Circle.t -> t
val segment : Segment.t -> t
val polygon : Polygon.t -> t
val rect : Box.t -> t

(* *)
val translate : Vec.t -> t -> t
val rotate : ?center:Point.t -> angle:float -> t -> t

(* *)

val center : t -> Point.t
val signed_area : t -> float
val distance2 : Point.t -> t -> float
val mem : Point.t -> t -> bool
val intersects : t -> t -> bool
val intersections : t -> t -> Point.t list
val nearest_points : Point.t -> t -> (Point.t * Vec.t) list

(* *)

val separation_axis : t -> t -> Vec.t option
val contact_points : t -> t -> float * Point.t list