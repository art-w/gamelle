module Bitmap : sig
  type t

  val load : string -> t
  val rotate : float -> t -> t
  val scale : float -> t -> t
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

val clock : unit -> float
val dt : unit -> float
val draw : Bitmap.t -> float -> float -> unit
val set_color : int -> unit
val draw_line : float * float -> float * float -> unit
val draw_rect : float * float -> float * float -> unit
val fill_rect : float * float -> float * float -> unit
val draw_poly : (float * float) list -> unit
val fill_poly : (float * float) list -> unit
val draw_circle : float * float -> float -> unit
val fill_circle : float * float -> float -> unit
val draw_thick_line : stroke:float -> float * float -> float * float -> unit
val draw_string : Font.t -> size:int -> string -> float -> float -> unit

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

  val is_pressed : t -> key -> bool
  val mouse_pos : t -> float * float
end

val window_size : unit -> float * float

val run :
  'state ->
  update:(Event.t -> 'state -> 'state) ->
  render:('state -> unit) ->
  unit
