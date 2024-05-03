(** Gamelle is a tiny 2D game engine. It provides:

- Hot code reload on every file change for a quick feedback loop
- Export to a single HTML file to share your game online
- Automatic assets loading for images, sounds and fonts: see {!section:Assets}
- Immediate mode GUI to quickly put together menus and widgets: see {!Ui}
- Collisions detection and rigid physics: see {!Physics}

{1 Getting started}

You can create a new game project with:

{@shell[
$ gamelle init mygame
$ cd mygame
$ make
]}

The [make] command starts the game in development mode: Editing [src/mygame.ml] will automatically reload your game on every change. Game assets added to the folder [assets/] are automatically available through the dynamic [Assets] module.

To export your game as a single HTML file, compiled with [js_of_ocaml] and including all game assets:

{@shell[
$ make html
]}

*)

(** {1 Main loop} *)

type io
(** The type allowing input/output operations. Every side-effecting function requires a named argument [~io] of this type. *)

val run : 'state -> (io:io -> 'state -> 'state) -> unit
(** [run initial_state fn] is the game main loop. The function [fn] will
    be called at every frame to react to player inputs and draw the game state on the screen.

    {[
open Gamelle

type state = ...

let initial_state = ...

let () =
  Gamelle.run initial_state @@ fun ~io current_state ->
    let new_state = (* TODO: react to player inputs and update the current state *) in
    (* TODO: draw the new game state *)
    new_state
]}

*)

(** {1 Maths} *)

(** {2 Linear Algebra} *)

(** Gamelle extends the {!Gg} maths library. *)

module Color : sig
  (** Colors. *)

  type t = Gg.color
  (** The type of colors. *)

  val rgb : ?alpha:float -> int -> int -> int -> t
  val v : float -> float -> float -> float -> t

  (** {1 Transformations} *)

  val with_a : t -> float -> t

  (** {1 Basic colors} *)

  val red : t
  val green : t
  val blue : t
  val white : t
  val black : t
end

module Transform : sig
  (** Rotations, scaling, translations. *)

  type t

  val default : t
  val translate : Gg.v2 -> t -> t
  val scale : float -> t -> t
  val rotate : float -> t -> t
  val project : t -> Gg.p2 -> Gg.p2
end

module Point : sig
  (** Points: [x] and [y] positions in 2D. *)

  type t = Gg.p2
  (** The type of positions. *)

  include module type of Gg.P2 with type t := t

  val lerp : float -> t -> t -> t
  (** [lerp t a b] is the linear interpolation between the two points [a] and [b] controlled by the time parameter [t]. *)
end

module Vec : sig
  (** Vectors: directions in 2D. *)

  type t = Gg.v2
  (** The type of directional vectors. *)

  val zero : t
  val v : float -> float -> t
  val x : t -> float
  val y : t -> float
  val to_tuple : t -> float * float
  val norm : t -> float
  val unit : t -> t
  val dot : t -> t -> float
  val ( * ) : float -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
end

(** {2 Geometry} *)

module Size : module type of Draw_geometry.Size
(** Sizes: [width] and [height] dimensions. *)

module Segment : sig
  (** Segments connecting two {!Point}s. *)

  type t

  val v : Point.t -> Point.t -> t
  val to_tuple : t -> Point.t * Point.t
  val vector : t -> Vec.t
  val draw : io:io -> ?color:Color.t -> t -> unit
  val intersect : t -> t -> bool
end

module Box : sig
  (** Axis-aligned bounding boxes. *)

  type t = Gg.box2
  (** The type of axis-aligned bounding boxes (rectangles without a rotation). *)

  val zero : t
  val v : Point.t -> Size.t -> t
  val o : t -> Point.t
  val ox : t -> float
  val oy : t -> float
  val size : t -> Size.t
  val w : t -> float
  val h : t -> float
  val sides : t -> Segment.t * Segment.t * Segment.t * Segment.t
  val mem : Point.t -> t -> bool
  val random_mem : t -> Point.t
  val midx : t -> float
  val midy : t -> float
  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Circle : sig
  (** Circles. *)

  include module type of Draw_geometry.Circle

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Polygon : sig
  (** Polygons. *)

  include module type of Draw_geometry.Polygon

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

module Shape : sig
  (** Arbitrary shapes: circles and polygons. *)

  include module type of Draw_geometry.Shape

  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end

(** {1:Assets Assets} *)

(** Game assets like images, fonts and sounds which are added to the [assets/] folder are automatically loaded and available through the dynamic [Assets] module. For example, a bitmap file [assets/foo.png] is accessible as [Assets.foo : Bitmap.t]. *)

(** {3 Images} *)

module Bitmap : sig
  (** Bitmap images: PNG, JPEG. *)

  type t
  (** The type of bitmap images (png, jpeg). *)

  val sub : t -> int -> int -> int -> int -> t
  (** [sub img x y w h] returns the sub-image at position [x,y] and size [w,h]. *)

  (**/**)

  val load : string -> t
end

val draw : io:io -> at:Point.t -> Bitmap.t -> unit
(** [draw ~io ~at bitmap] draws the image [bitmap] at position [at] on the screen. *)

(** {3 Text} *)

module Font : sig
  (** Typefaces used to render text on screen. *)

  type t
  (** The type of fonts (ttf). *)

  val default : t
  (** The default font. *)

  val default_size : int
  (** The default font size. *)

  (**/**)

  val load : string -> t
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
    io:io ->
    ?color:Color.t ->
    ?font:Font.t ->
    ?size:int ->
    at:Point.t ->
    t ->
    unit

  val draw :
    io:io ->
    ?color:Color.t ->
    ?font:Font.t ->
    ?size:int ->
    at:Point.t ->
    string ->
    unit

  val size_t : io:io -> ?font:Font.t -> ?size:int -> t -> Size.t
  val size : io:io -> ?font:Font.t -> ?size:int -> string -> Size.t

  val size_multiline :
    io:io ->
    ?width:float ->
    ?interline:float ->
    ?font:Font.t ->
    ?size:int ->
    string ->
    Size.t

  val size_multiline_t :
    io:io ->
    ?width:float ->
    ?interline:float ->
    ?font:Font.t ->
    ?size:int ->
    t ->
    Size.t

  val draw_multiline_t :
    io:io ->
    ?color:Color.t ->
    ?width:float ->
    ?interline:float ->
    ?font:Font.t ->
    ?size:int ->
    at:Point.t ->
    t ->
    unit

  val draw_multiline :
    io:io ->
    ?color:Color.t ->
    ?width:float ->
    ?interline:float ->
    ?font:Font.t ->
    ?size:int ->
    at:Point.t ->
    string ->
    unit
end

val draw_string :
  io:io ->
  ?color:Color.t ->
  ?font:Font.t ->
  ?size:int ->
  at:Point.t ->
  string ->
  unit
(** [draw_string ~io ~at txt] prints the string [txt] at position [at] on the screen.

- [?color] is the text color, see {!View.color}
- [?font] is the typeface used to render the text, see {!View.font}
- [?size] is the font size, see {!View.font_size}

 *)

(** {3 Audio} *)

module Sound : sig
  (** Audio sounds and musics: MP3, OGG. *)

  type sound

  val play : io:io -> sound -> unit

  type music

  val play_music : io:io -> music -> unit

  (**/**)

  val load : string -> sound
  val load_music : string -> music
end

(** {1 Player inputs} *)

module Input : sig
  (** Player inputs: mouse and keyboard events. *)

  val mouse_pos : io:io -> Point.t
  val wheel_delta : io:io -> float

  type key =
    [ `alt
    | `alt_gr
    | `arrow_down
    | `arrow_left
    | `arrow_right
    | `arrow_up
    | `backspace
    | `click_left
    | `click_right
    | `control_left
    | `control_right
    | `delete
    | `escape
    | `input_char of string
    | `meta
    | `physical_char of char
    | `quit
    | `shift
    | `space
    | `tab
    | `wheel
    | `unknown_key ]

  val is_pressed : io:io -> key -> bool
  val is_up : io:io -> key -> bool
  val is_down : io:io -> key -> bool
end

module Ui : Ui.S with type io := io
(** Graphical user interface: buttons, checkboxes, text inputs. *)

(** {1 Camera} *)

module View : sig
  (** Customize the {!io} camera, default color and font used. *)

  val drawing_box : ?scale:bool -> ?set_window_size:bool -> Box.t -> io -> io
  val translate : Vec.t -> io -> io
  val scale : float -> io -> io
  val rotate : float -> io -> io
  val clip : Box.t -> io -> io
  val unclip : io -> io
  val clip_events : bool -> io -> io
  val z_index : int -> io -> io
  val color : Color.t -> io -> io
  val font : Font.t -> io -> io
  val font_size : int -> io -> io
end

module Window : sig
  (** Configure the game window. *)

  val show_cursor : io:io -> bool -> unit
  val set_size : io:io -> Size.t -> unit
  val size : io:io -> Size.t
  val box : io:io -> Box.t
end

(** {1 Animations} *)

val clock : io:io -> float
(** [clock ~io] returns the number of elapsed seconds since the game started until the current frame. The clock is defined at the beginning of a frame, calling [clock] multiple times will always produce the same result. *)

val dt : io:io -> float
(** [dt ~io] is the duration of a frame, which is fixed to a 60fps framerate. *)

module Ease : sig
  (** Easing functions, to smooth changes over time.

   - [in_] functions are smooth near zero.
   - [out_] functions are smooth near one.
   - [in_out_] functions are smooth on both ends.

  *)

  type t = float -> float
  (** The type of easing functions, with input and output varying from [0.0] to [1.0]. *)

  val linear : t
  (** The [linear] identity, providing no easing at all. *)

  (** {1 Quadratic} *)

  val in_quad : t
  val out_quad : t
  val in_out_quad : t

  (** {1 Cubic} *)

  val in_cubic : t
  val out_cubic : t
  val in_out_cubic : t

  (** {1 Quartic} *)

  val in_quart : t
  val out_quart : t
  val in_out_quart : t

  (** {1 Backward overshoot} *)

  val in_back : t
  val out_back : t
  val in_out_back : t

  (** {1 Bounce} *)

  val in_bounce : t
  val out_bounce : t
  val in_out_bounce : t

  (** {1 Elastic overshoot} *)

  val in_elastic : t
  val out_elastic : t
  val in_out_elastic : t
end

module Anim : sig
  (** Animations. *)

  type 'a t
  (** The type of animations describing an ['a] value varying over time. *)

  val v : float -> (float -> 'a) -> 'a t
  (** [v duration fn] is an animation, lasting [duration] in seconds. The function [fn] will be called on demand to compute the current ['a] value. *)

  val const : float -> 'a -> 'a t
  (** [const duration x] is a constant animation always producing [x], lasting [duration] seconds. *)

  val get : 'a t -> 'a
  (** [get t] returns the current ['a] value. *)

  val last : 'a t -> 'a
  (** [last t] returns the last ['a] value, at the end of the animation. *)

  val duration : 'a t -> float
  (** [duration t] is the length of the animation [t]. Returns [0.0] if the animation has completed. *)

  val update : dt:float -> 'a t -> 'a t
  (** [update ~dt t] moves the animation [t] forward in time by [dt] seconds. See {!dt}. *)

  val seq : 'a t -> 'a t -> 'a t
  (** [seq a b] is the animation [a] followed by the animation [b]. *)

  val ( >> ) : 'a t -> 'a t -> 'a t
  (** [a >> b] is the same as [seq a b]. *)

  val continue : 'a t -> ('a -> 'a t) -> 'a t
  (** [continue t fn] is the same as [seq a (fn (last t))]. It continues the animation [t] with a follow-up animation depending on the last value of [t]. *)

  val ( >>- ) : 'a t -> ('a -> 'a t) -> 'a t
  (** [t >>- fn] is the same as [continue t fn]. *)

  val ( let> ) : 'a t -> ('a -> 'a t) -> 'a t
  (** [let> v = t in ...] is the same as [continue t (fun v -> ...)]. *)

  val rev : 'a t -> 'a t
  (** [rev t] returns the animation [t] playing backward. *)

  val scale : float -> 'a t -> 'a t
  (** [scale f t] stretches the animation [t] duration by a factor [f]. *)

  val cut : float -> 'a t -> 'a t * 'a t
  (** [cut duration t] returns the animation [t] truncated to [duration], and the rest. *)

  val frames : float -> 'a array -> 'a t
  (** [frames duration arr] is an animation, lasting [duration] seconds, which steps over the values of the array [arr]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn t] applies [fn] to each output value of the animation [t]. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 fn ta tb] applies [fn] to each animation [ta] and [tb] values. *)
end

module Physics : sig
  (** Rigid physics for {!Shape} objects. *)

  type t
  type kind = Movable | Immovable

  val make :
    ?mass:float ->
    ?inertia:float ->
    ?restitution:float ->
    ?kind:kind ->
    Shape.t ->
    t

  val center : t -> Point.t
  val add_velocity : Vec.t -> t -> t
  val add_rot_velocity : float -> t -> t
  val update : dt:float -> t -> t
  val fix_collisions : t list -> t list
  val draw : io:io -> ?color:Color.t -> t -> unit
  val fill : io:io -> ?color:Color.t -> t -> unit
end
