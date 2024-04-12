open Gamelle_common
open Geometry

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
val show_cursor : bool -> unit

val draw_string :
  io:io -> color:Color.t -> ?font:Font.t -> ?size:int -> string -> point -> unit

val text_size : io:io -> ?font:Font.t -> ?size:int -> string -> size

module Window : sig
  val set_size : size -> unit
  val size : unit -> size
  val box : unit -> box
end

(* *)
