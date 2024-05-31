open Gamelle_common
open Geometry

type io_backend
type io = io_backend Gamelle_common.abstract_io

val run : 'state -> (io:io -> 'state -> 'state) -> unit

module Bitmap : sig
  type t

  val load : string -> t
  val sub : t -> int -> int -> int -> int -> t
end

module Font : sig
  type t

  val default : t
  val default_size : int
  val load : string -> t
  val set_font : t -> io_backend -> io_backend
  val set_font_size : int -> io_backend -> io_backend
end

module Sound : sig
  type t
  type playing
  val start_playing : io:io -> t -> playing

  val continue_playing : io:io -> playing -> bool

  val duration : t -> float

  val sound_of_playing : playing -> t

  val elapsed_duration : playing -> float

  val load : string -> t
  val play : io:io -> t -> unit
  val play_music : io:io -> t -> unit
  val stop_music : io:io -> unit
end

val clock : io:io -> float
val dt : io:io -> float
val draw : io:io -> Bitmap.t -> point -> unit
val draw_line : io:io -> ?color:Color.t -> Segment.t -> unit
val draw_rect : io:io -> ?color:Color.t -> box -> unit
val fill_rect : io:io -> ?color:Color.t -> box -> unit
val draw_poly : io:io -> ?color:Color.t -> Polygon.t -> unit
val fill_poly : io:io -> ?color:Color.t -> Polygon.t -> unit
val draw_circle : io:io -> ?color:Color.t -> Circle.t -> unit
val fill_circle : io:io -> ?color:Color.t -> Circle.t -> unit

module Text : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val ( ^ ) : t -> t -> t
  val slice : ?start:int -> ?stop:int -> t -> t
  val length : t -> int
  val get : t -> int -> t
  val chars : t -> t list

  val draw :
    io:io ->
    ?color:Color.t ->
    ?font:Font.t ->
    ?size:int ->
    at:point ->
    t ->
    unit

  val size : io:io -> ?font:Font.t -> ?size:int -> t -> size
end

module Window : sig
  val show_cursor : io:io -> bool -> unit
  val size : io:io -> Size.t
end
