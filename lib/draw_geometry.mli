open Gamelle_common.Geometry
open Gamelle_backend

type bitmap := Bitmap_.t
type font := Gamelle_backend.Font.t

module Color : sig
  include module type of Color
  (** @inline *)
end

type color = Color.t

module Point : sig
  include module type of Point
end

type point = Point.t

module Vec : sig
  include module type of Vec
end

type vec = Vec.t

module Segment : sig
  include module type of Segment

  val draw : io:io -> ?color:Color.t -> t -> unit
end

type segment = Segment.t

module Size : module type of Size

type size = Size.t

module Box : sig
  include module type of Box

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

type box = Box.t

module Circle : sig
  include module type of Circle

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

type circle = Circle.t

module Polygon : sig
  include module type of Polygon

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

type polygon = Polygon.t

module Shape : sig
  include module type of Shape

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

type shape = Shape.t

module Text : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val ( ^ ) : t -> t -> t
  val length : t -> int
  val sub : t -> int -> int -> t
  val split_on_char : char -> t -> t list
  val concat : t list -> t

  val draw_t :
    io:io -> ?color:color -> ?font:font -> ?size:int -> at:point -> t -> unit

  val draw :
    io:io ->
    ?color:color ->
    ?font:font ->
    ?size:int ->
    at:point ->
    string ->
    unit

  val size_t : io:io -> ?font:font -> ?size:int -> t -> size
  val size : io:io -> ?font:font -> ?size:int -> string -> size

  val size_multiline :
    io:io ->
    ?width:float ->
    ?interline:float ->
    ?font:font ->
    ?size:int ->
    string ->
    size

  val size_multiline_t :
    io:io ->
    ?width:float ->
    ?interline:float ->
    ?font:font ->
    ?size:int ->
    t ->
    size

  val draw_multiline_t :
    io:io ->
    ?color:color ->
    ?width:float ->
    ?interline:float ->
    ?font:font ->
    ?size:int ->
    at:point ->
    t ->
    unit

  val draw_multiline :
    io:io ->
    ?color:color ->
    ?width:float ->
    ?interline:float ->
    ?font:font ->
    ?size:int ->
    at:point ->
    string ->
    unit
end

val draw : io:io -> at:point -> bitmap -> unit
