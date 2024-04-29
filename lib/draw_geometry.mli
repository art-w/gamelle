open Gamelle_common
open Gamelle_common.Geometry
open Gamelle_backend
module Size : module type of Size
module Size1 : module type of Size1

type color = Geometry.color
type point = Geometry.point
type vec = Geometry.vec
type segment = Geometry.segment
type circle = Geometry.circle
type box = Geometry.box
type size = Geometry.size

module Point : sig
  include module type of Point
end

module Polygon : sig
  include module type of Polygon

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Shape : sig
  include module type of Shape

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Vec : sig
  include module type of Vec
end

module Color : sig
  include module type of Color
end

module Segment : sig
  include module type of Segment

  val draw : io:io -> ?color:Color.t -> t -> unit
end

module Circle : sig
  include module type of Circle

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Box : sig
  include module type of Box

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Text : sig
  include module type of Text

  val sub : t -> int -> int -> t
  val split_on_char : char -> t -> t list
  val concat : t list -> t

  val draw_t :
    io:Gamelle_backend.io ->
    ?color:color ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    at:point ->
    t ->
    unit

  val draw :
    io:Gamelle_backend.io ->
    ?color:color ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    at:point ->
    string ->
    unit

  val size_t :
    io:Gamelle_backend.io ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    t ->
    size

  val size :
    io:Gamelle_backend.io ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    string ->
    size

  val size_multiline :
    io:Gamelle_backend.io ->
    ?width:float ->
    ?interline:float ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    string ->
    size

  val size_multiline_t :
    io:Gamelle_backend.io ->
    ?width:float ->
    ?interline:float ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    t ->
    size

  val draw_multiline_t :
    io:Gamelle_backend.io ->
    ?color:color ->
    ?width:float ->
    ?interline:float ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    at:point ->
    t ->
    unit

  val draw_multiline :
    io:Gamelle_backend.io ->
    ?color:color ->
    ?width:float ->
    ?interline:float ->
    ?font:Gamelle_backend.Font.t ->
    ?size:int ->
    at:point ->
    string ->
    unit
end

val draw : io:io -> Bitmap.t -> size -> unit
