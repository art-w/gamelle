open Gg
module Color : module type of Color

module Bitmap : sig
  type t

  val load : string -> t
  val sub : t -> int -> int -> int -> int -> t
end

module Font : sig
  type t

  val load : string -> t
  val draw : t -> int -> string -> Bitmap.t
end

module Sound : sig
  type sound

  val load : string -> sound
  val play : sound -> unit

  type music

  val load_music : string -> music
  val play_music : music -> unit
end

module View : sig
  type t
  type 'a scene = view:t -> 'a

  val ( & ) : unit scene -> unit scene -> unit scene
  val translate : float * float -> 'a scene -> 'a scene
  val scale : float -> 'a scene -> 'a scene
  val rotate : float -> 'a scene -> 'a scene
  val default : t
  val translated : float * float -> t -> t
  val scaled : float -> t -> t
  val rotated : float -> t -> t
end

val clock : unit -> float
val dt : unit -> float
val draw : view:View.t -> Bitmap.t -> p2 -> unit
val draw_line : view:View.t -> color:Color.t -> p2 -> p2 -> unit
val draw_rect : view:View.t -> color:Color.t -> p2 -> size2 -> unit
val fill_rect : view:View.t -> color:Color.t -> p2 -> size2 -> unit
val draw_poly : view:View.t -> color:Color.t -> p2 list -> unit
val fill_poly : view:View.t -> color:Color.t -> p2 list -> unit
val draw_circle : view:View.t -> color:Color.t -> p2 -> size1 -> unit
val fill_circle : view:View.t -> color:Color.t -> p2 -> size1 -> unit
val show_cursor : bool -> unit

val draw_string :
  view:View.t -> color:Color.t -> Font.t -> size:int -> string -> p2 -> unit

module Event : sig
  type t

  type key =
    | Escape
    | Control_left
    | Control_right
    | Arrow_left
    | Arrow_right
    | Arrow_up
    | Arrow_down
    | Char of char
    | Click_left
    | Click_right
    | Wheel_up
    | Wheel_down

  val is_pressed : t -> key -> bool
  val mouse_pos : t -> float * float
end

val window_size : unit -> size2

val run :
  ?on_exit:('state -> unit) ->
  'state ->
  update:(Event.t -> 'state -> 'state) ->
  render:(view:View.t -> 'state -> unit) ->
  unit
