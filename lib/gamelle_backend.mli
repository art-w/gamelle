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
  val draw : color:Color.t -> t -> int -> string -> Bitmap.t
end

module Sound : sig
  type sound

  val load : string -> sound
  val play : io:io -> sound -> unit

  type music

  val load_music : string -> music
  val play_music : io:io -> music -> unit
end

val clock : unit -> float
val dt : unit -> float
val draw : io:io -> Bitmap.t -> point -> unit
val draw_line : io:io -> color:Color.t -> Segment.t -> unit
val draw_rect : io:io -> color:Color.t -> box -> unit
val fill_rect : io:io -> color:Color.t -> box -> unit
val draw_poly : io:io -> color:Color.t -> point list -> unit
val fill_poly : io:io -> color:Color.t -> point list -> unit
val draw_circle : io:io -> color:Color.t -> Circle.t -> unit
val fill_circle : io:io -> color:Color.t -> Circle.t -> unit
val show_cursor : io:io -> bool -> unit

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
    io:io -> color:Color.t -> ?font:Font.t -> ?size:int -> t -> point -> unit

  val size : io:io -> ?font:Font.t -> ?size:int -> t -> size
end

module Window : sig
  val set_size : io:io -> size -> unit
  val size : io:io -> size
  val box : io:io -> box
end

(* *)
