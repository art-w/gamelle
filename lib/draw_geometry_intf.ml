open Gamelle_common
open Gamelle_common.Geometry

module type S = sig
  type io
  type bitmap
  type font

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

  val draw : io:io -> bitmap -> size -> unit
end
