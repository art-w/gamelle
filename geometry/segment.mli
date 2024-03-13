open Gg

type t

val v : p2 -> p2 -> t
val start : t -> p2
val end_ : t -> p2
val to_tuple : t -> p2 * p2
val vector : t -> v2
val intersection : t -> t -> p2 option
val intersect : t -> t -> bool
val equal : t -> t -> bool
val translate : t -> v2 -> t
