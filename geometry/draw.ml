open Gg

module type S = sig
  type view

  val draw_line : view:view -> color:Color.t -> Segment.t -> unit
  val draw_rect : view:view -> color:Color.t -> box2 -> unit
  val fill_rect : view:view -> color:Color.t -> box2 -> unit
  val draw_poly : view:view -> color:Color.t -> p2 list -> unit
  val fill_poly : view:view -> color:Color.t -> p2 list -> unit
  val draw_circle : view:view -> color:Color.t -> Circle.t -> unit
  val fill_circle : view:view -> color:Color.t -> Circle.t -> unit
end
