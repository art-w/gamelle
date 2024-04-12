open Gg

module type S = sig
  type io

  val draw_line : io:io -> color:Color.t -> Segment.t -> unit
  val draw_rect : io:io -> color:Color.t -> box2 -> unit
  val fill_rect : io:io -> color:Color.t -> box2 -> unit
  val draw_poly : io:io -> color:Color.t -> p2 list -> unit
  val fill_poly : io:io -> color:Color.t -> p2 list -> unit
  val draw_circle : io:io -> color:Color.t -> Circle.t -> unit
  val fill_circle : io:io -> color:Color.t -> Circle.t -> unit
end
