open Gg
include module type of Gg.Box2

val top : t -> Segment.t
val left : t -> Segment.t
val right : t -> Segment.t
val bottom : t -> Segment.t
val sides : t -> Segment.t * Segment.t * Segment.t * Segment.t
val random_mem : t -> p2
