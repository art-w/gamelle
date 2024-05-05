open Gg
include module type of Gg.Box2

val top : t -> Segment.t
val left : t -> Segment.t
val right : t -> Segment.t
val bottom : t -> Segment.t
val random_mem : t -> p2
val translate : t -> v2 -> t

val centered : t -> t -> t
(** [centered b1 b2] is [b1] centered inside [b2]. *)

val v_corners : p2 -> p2 -> t
val x_left : t -> float
val x_right : t -> float
val x_middle : t -> float
val y_top : t -> float
val y_bottom : t -> float
val y_middle : t -> float
