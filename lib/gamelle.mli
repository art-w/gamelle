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

module Color : sig
  (** Colors. *)

  type t = Gg.color
  (** The type of colors. *)

  val v : float -> float -> float -> float -> t
  (** [v r g b a] is the color with alpha transparency [a] and RGB components between [0.0] and [1.0] *)

  val rgb : ?alpha:float -> int -> int -> int -> t
  (** [rgb r g b] is the RGB color with components red [r], green [g] and blue [b] between [0] and [255]. *)

  val hsl : ?alpha:float -> float -> float -> float -> t
  (** [hsl h s l] returns a color from the hue [h] (between [0.0] and [360.0]), the saturation [s] and lightness [l] (between [0.0] and [1.0]). *)

  (** {1 Transformations} *)

  val with_alpha : float -> t -> t
  (** [with_alpha a c] returns the color [c] with its alpha transparency set to [a]. *)

  (** {1 Basic colors} *)

  val red : t
  val green : t
  val blue : t
  val white : t
  val black : t
  val yellow : t
  val cyan : t
  val magenta : t
  val gray : t
  val orange : t
end

type xy = { x : float; y : float }
(** The type of points and vectors: [x] and [y] coordinates. *)

module Point : sig
  (** Points: [x] and [y] positions in 2D. *)

  type t = xy
  (** The type of positions. *)

  val v : float -> float -> t
  (** [v x y] returns a point at coordinates [x] and [y]. *)

  val zero : t
  (** [zero] is the point at the origin [v 0.0 0.0] *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io pt] draws the point [pt] on the screen. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the point [t] coordinates. *)

  (** {2 Accessors} *)

  val x : t -> float
  (** [x pt] returns the [x] coordinate of the point [pt]. *)

  val y : t -> float
  (** [y pt] returns the [y] coordinate of the point [pt]. *)

  val lerp : float -> t -> t -> t
  (** [lerp t a b] is the linear interpolation between the two points [a] and [b] controlled by the time parameter [t].

      - [lerp 0.0 a b = a]
      - [lerp 1.0 a b = b]
      - [lerp 0.5 a b =] mid point of [a] and [b]
  *)
end

module Vec : sig
  (** Vectors: directions in 2D. *)

  type t = xy
  (** The type of directional vectors. *)

  val v : float -> float -> t
  (** [v x y] is a vector with direction [x], [y]. *)

  val zero : t
  (** [zero] is the zero vector [v 0.0 0.0]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> at:Point.t -> t -> unit
  (** [draw ~io ~at vec] draws an arrow with origin [at] and direction [vec]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the vector [t] coordinates. *)

  (** {2 Accessors} *)

  val x : t -> float
  (** [x v] is the [x] component of the vector [v]. *)

  val y : t -> float
  (** [y v] is the [y] component of the vector [v]. *)

  val to_tuple : t -> float * float
  (** [to_tuple v] is [x v, y v]. *)

  (** {2 Linear algebra} *)

  val norm : t -> float
  (** [norm v] is the length of the vector [v]. *)

  val unit : t -> t
  (** [unit v] is the vector [v] normalized to have a unit length. *)

  val dot : t -> t -> float
  (** [dot a b] is the dot product of the vectors [a], [b]. *)

  (** {2 Operators} *)

  val ( * ) : float -> t -> t
  (** [f * v] is the vector [v] scaled by a factor [f]. *)

  val ( + ) : t -> t -> t
  (** [a + b] is the sum of the two vectors [a], [b]. *)

  val ( - ) : t -> t -> t
  (** [a - b] is the substraction of vector [a] by [b]. *)
end

(** {2 Geometry} *)

module Size : sig
  (** Sizes: [width] and [height] dimensions. *)

  type t = xy
  (** The type of sizes in 2D. *)

  val v : float -> float -> t
  (** [v w h] is a size with width [w] and height [h]. *)

  (** {2 Accessors} *)

  val width : t -> float
  (** [width s] is the width of [s]. *)

  val height : t -> float
  (** [height s] is the height of [s]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> at:Point.t -> t -> unit
  (** [draw ~io ~at t] draws the box with top-left corner [at] and size [t]. *)

  val fill : io:io -> ?color:Color.t -> at:Point.t -> t -> unit
  (** [fill ~io ~at t] fills the box with top-left [at] and size [t]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the size [t] dimensions. *)
end

module Segment : sig
  (** Segments connecting two {!Point}s. *)

  type t
  (** The type of segments. *)

  val v : Point.t -> Point.t -> t
  (** [v start stop] is a segment connecting point [start] to point [stop]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io s] draws the segment on the screen. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the segment [t] end-points. *)

  (** {2 Accessors} *)

  val to_tuple : t -> Point.t * Point.t
  (** [to_tuple s] returns the two end-points of the segment [s]. *)

  val vector : t -> Vec.t
  (** [vector s] is the direction vector of the segment [s]. *)

  val intersect : t -> t -> bool
  (** [intersect a b] returns [true] if segment [a] intersects segment [b], [false] otherwise. *)
end

module Box : sig
  (** Axis-aligned bounding boxes. *)

  type t
  (** The type of axis-aligned bounding boxes (rectangles without a rotation). *)

  val v : Point.t -> Size.t -> t
  (** [v topleft size] is a box with origin point [topleft] and dimension [size]. *)

  val v_center : Point.t -> Size.t -> t
  (** [v_center p size] is a box with center point [p] and dimension [size]. *)

  val v_corners : Point.t -> Size.t -> t
  (** [v_corner topleft bottomright] is a box with origin point [topleft] and bottom right point [bottomright]. *)

  val zero : t
  (** [zero] is the empty box located at the origin. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io b] draws the outline border of the box [b]. *)

  val fill : io:io -> ?color:Color.t -> t -> unit
  (** [fill ~io b] fills the inside of the box [b]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the box [t] shape. *)

  (** {2 Accessors} *)

  val size : t -> Size.t
  (** [size b] returns the dimension of the box [b]. *)

  val top_left : t -> Point.t
  (** [top_left b] returns the top-left point of the box [b]. *)

  val top_right : t -> Point.t
  (** [top_right b] returns the top-right point of the box [b]. *)

  val bottom_left : t -> Point.t
  (** [bottom_left b] returns the bottom-left point of the box [b]. *)

  val bottom_right : t -> Point.t
  (** [bottom_right b] returns the bottom-right point of the box [b]. *)

  val x_left : t -> float
  (** [x_left b] is the left-most [x] coordinate of the box [b]. *)

  val x_middle : t -> float
  (** [x_middle b] is the center [x] coordinate of the box [b]. *)

  val x_right : t -> float
  (** [x_right b] is the right-most [x] coordinate of the box [b]. *)

  val y_top : t -> float
  (** [y_top b] is the upper [y] coordinate of the box [b]. *)

  val y_middle : t -> float
  (** [y_middle b] is the center [y] coordinate of the box [b]. *)

  val y_bottom : t -> float
  (** [y_bottom b] is the lower [y] coordinate of the box [b]. *)

  val left : t -> Segment.t
  (** [left b] returns the left segment of the box [b]. *)

  val right : t -> Segment.t
  (** [right b] returns the right segment of the box [b]. *)

  val top : t -> Segment.t
  (** [top b] returns the top segment of the box [b]. *)

  val bottom : t -> Segment.t
  (** [bottom b] returns the bottom segment of the box [b]. *)

  (** {2 Collisions} *)

  val mem : Point.t -> t -> bool
  (** [mem pt b] returns [true] if the point [pt] lies inside the box [b], [false] otherwise. *)

  val random_mem : t -> Point.t
  (** [random_mem b] returns a random point inside the box [b]. *)
end

module Circle : sig
  (** Circles. *)

  type t
  (** The type of circles. *)

  val v : Point.t -> float -> t
  (** [v pt r] is a circle with center point [pt] and radius [r]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io c] draws the outline border of the circle [c]. *)

  val fill : io:io -> ?color:Color.t -> t -> unit
  (** [fill ~io c] fills the inside of the circle [c]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the circle [t] shape. *)

  (** {2 Accessors} *)

  val center : t -> Point.t
  (** [center c] is the center point of the circle [c]. *)

  val radius : t -> float
  (** [radius c] is the radius of the circle [c]. *)

  (** {2 Transforms} *)

  val translate : Vec.t -> t -> t
  (** [translate v c] translates the circle [c] by vector [v]. *)

  (* val scale : float -> t -> t *)
  (** [scale f c] scales the circle [c] by factor [f]. *)

  (** {2 Collisions} *)

  val mem : Point.t -> t -> bool
  (** [mem pt c] returns [true] if point [pt] lies inside the circle [c], [false] otherwise. *)

  val intersect : t -> t -> bool
  (** [intersect a b] returns [true] if circle [a] intersects circle [b], [false] otherwise. *)

  val intersections : t -> t -> Point.t list
  (** [intersections a b] returns the list of intersections points between the circles [a] and [b]. *)
end

module Polygon : sig
  (** Polygons. *)

  type t
  (** The type of polygons. *)

  val v : Point.t list -> t
  (** [v pts] is a polygon with vertices [pts]. *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io p] draws the outline border of the polygon [p]. *)

  val fill : io:io -> ?color:Color.t -> t -> unit
  (** [fill ~io p] fills the inside of the polygon [p]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the polygon [t] points. *)

  (** {2 Accessors} *)

  val center : t -> Point.t
  (** [center p] returns the center of mass of the polygon [p]. *)

  val points : t -> Point.t list
  (** [segments p] returns the list of vertices of the polygon [p]. *)

  val segments : t -> Segment.t list
  (** [segments p] returns the list of segments of the polygon [p]. *)

  val bounding_box : t -> Box.t
  (** [bounding_box p] returns the bounding box of the polygon [p]. *)

  (** {2 Transforms} *)

  val translate : Vec.t -> t -> t
  (** [translate v p] translates the polygon [p] by vector [v]. *)
end

module Shape : sig
  (** Arbitrary shapes: segments, circles and polygons. *)

  type t
  (** The type of shapes: segments, circles, polygons. *)

  val segment : Segment.t -> t
  (** [segment s] is the shape representing the segment [s]. *)

  val circle : Circle.t -> t
  (** [circle c] is the shape representing the circle [c]. *)

  val rect : Box.t -> t
  (** [rect b] is the shape representing the rectangle [b]. *)

  val polygon : Polygon.t -> t
  (** [polygon p] is the shape representing the polygon [p]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io s] draws the outline border of the shape [s]. *)

  val fill : io:io -> ?color:Color.t -> t -> unit
  (** [fill ~io p] fills the inside of the shape [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [Format.printf "%a" pp t] pretty prints the shape [t]. *)

  (** {2 Accessors} *)

  val center : t -> Point.t
  (** [center s] returns the center of mass of the shape [s]. *)

  val signed_area : t -> float
  (** [signed_area s] returns the signed area of the shape [s]. *)

  (** {2 Transform} *)

  val rotate : ?center:Point.t -> float -> t -> t
  (** [rotate a s] rotates the shape [s] by angle [a] (in radians). *)

  (** {2 Collisions} *)

  val mem : Point.t -> t -> bool
  (** [mem pt s] returns [true] if point [pt] lies inside the shape [s], [false] otherwise. *)

  val distance2 : Point.t -> t -> float
  (** [distance2 pt s] returns the squared distance of the point [pt] with the shape [s]. *)

  val intersect : t -> t -> bool
  (** [intersect a b] returns [true] if the shape [a] intersects the shape [b], [false] otherwise. *)

  val intersections : t -> t -> Point.t list
  (** [intersections a b] returns the list of intersections points between the shapes [a] and [b]. *)
end

(** {1:Assets Assets} *)

(** Game assets like images, fonts and sounds which are added to the [assets/] folder are automatically loaded and available through the dynamic [Assets] module. For example, a bitmap file [assets/foo.png] is accessible as [Assets.foo : Bitmap.t]. *)

(** {3 Images} *)

module Bitmap : sig
  (** Bitmap images: PNG, JPEG. *)

  type t
  (** The type of bitmap images (png, jpeg). *)

  val draw : io:io -> at:Point.t -> t -> unit
  (** [draw ~io ~at img] draws the image [img] at position [at] on the screen. *)

  (** {2 Accessors} *)

  val width : t -> int
  (** [width img] is the width in pixels of the image [img]. *)

  val height : t -> int
  (** [height img] is the height in pixels of the image [img]. *)

  val dimensions : t -> int * int
  (** [dimensions img] is the width and height dimensions of the image [img]. *)

  val size : t -> Size.t
  (** [size img] is the width and height size of the image [img]. Same as {!dimensions}. *)

  (** {2 Transforms} *)

  val sub : x:int -> y:int -> w:int -> h:int -> t -> t
  (** [sub ~x ~y ~w ~h img] returns the sub-image at position [x,y] and size [w,h]. *)

  (**/**)

  val load : w:int -> h:int -> string -> t
end

val draw : io:io -> at:Point.t -> Bitmap.t -> unit
(** [draw ~io ~at bitmap] draws the image [bitmap] at position [at] on the screen. Same as {!Bitmap.draw}.

Example:

{[
(* draw the assets/player.png bitmap at position x=100, y=200 *)
draw ~io Assets.player ~at:(Point.v 100. 200.) ;
]}

*)

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
  (** Text rendering. *)

  val draw :
    io:io ->
    ?color:Color.t ->
    ?font:Font.t ->
    ?size:int ->
    at:Point.t ->
    string ->
    unit
  (** [draw ~io ~at txt] prints the string [txt] at position [at] on the screen. *)

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
  (** [draw_multiline ~io ~at txt] prints the string [txt] at position [at] on the screen, possibly wrapping it on multiple lines if the text overflows [?width]. *)

  (** {2 Measure} *)

  val size : io:io -> ?font:Font.t -> ?size:int -> string -> Size.t
  (** [size ~io str] returns the size that would be used by {!draw} to render the text [str]. *)

  val size_multiline :
    io:io ->
    ?width:float ->
    ?interline:float ->
    ?font:Font.t ->
    ?size:int ->
    string ->
    Size.t
  (** [size_multiline ~io str] returns the size that would be used by {!draw_multiline} to render the text [str] on multiple lines. *)
end

val draw_string :
  io:io ->
  ?color:Color.t ->
  ?font:Font.t ->
  ?size:int ->
  at:Point.t ->
  string ->
  unit
(** [draw_string ~io ~at txt] prints the string [txt] at position [at] on the screen. Same as {!Text.draw}.

- [?color] is the text color, see {!View.color}
- [?font] is the typeface used to render the text, see {!val:View.font}
- [?size] is the font size, see {!View.font_size}

Examples:

{[
draw_string ~io "Hello World" ~at:(Input.mouse_pos ~io) ;
draw_string ~io ~color:Color.red ~at:(Point.v 200. 100.) "Bloody!" ;
draw_string ~io ~at:Point.zero "Why so serious?" ~font:Assets.comic_sans ~size:50 ;
]}

 *)

(** {3 Audio} *)

module Sound : sig
  (** Audio sounds and musics: MP3, OGG. *)

  type t
  (** The type of sounds and musics (mp3, ogg). *)

  val play : io:io -> t -> unit
  (** [play ~io t] plays the sound [t] once. *)

  val play_music : io:io -> t -> unit
  (** [play_music ~io t] plays the music [t] on a loop. If the music [t] was already playing, this function does nothing. Otherwise there can only be one music playing at a time, so the previous one is stopped. *)

  val stop_music : io:io -> unit
  (** [stop_music ~io] stops the currently playing music. *)

  (**/**)

  val load : string -> t
end

(** {1 Player inputs} *)

module Input : sig
  (** Player inputs: mouse and keyboard events. *)

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
  (** The type of player inputs. *)

  val is_pressed : io:io -> key -> bool
  (** [is_pressed ~io key] returns [true] if the player is currently holding [key]. *)

  val is_down : io:io -> key -> bool
  (** [is_down ~io key] returns [true] if the player just started pressing the [key]. *)

  val is_up : io:io -> key -> bool
  (** [is_up ~io key] returns [true] if the player was holding [key] and just released it. *)

  (** {2 Mouse} *)

  val mouse_pos : io:io -> Point.t
  (** [mouse_pos ~io] returns the coordinates of the player mouse. *)

  val wheel_delta : io:io -> float
  (** [wheel_delta ~io] returns the amount of change of the mouse wheel. *)
end

module Ui : sig
  (** Graphical user interface: buttons, checkboxes, text inputs. *)

  (** @inline *)
  include
    Ui.S
      with type io := io
       and type point := Point.t
       and type size := Size.t
       and type box := Box.t
end

(** {1 Camera} *)

module View : sig
  (** Customize the {!io} camera, default color and font used. *)

  (** {2 Viewport} *)

  val translate : Vec.t -> io -> io
  (** [translate v io] translates everything by vector [v]. *)

  val scale : float -> io -> io
  (** [scale f io] scales everything by a factor [f]. *)

  val rotate : float -> io -> io
  (** [rotate a io] rotates everything by angle [a]. *)

  val drawing_box : ?scale:bool -> ?set_window_size:bool -> Box.t -> io -> io
  (** [drawing_box b io] ensures the box [b] matches the dimensions of the window. See {!Window.size}. *)

  val clip : Box.t -> io -> io
  (** [clip b io] ensures no drawing can happen outside of the box [b]. *)

  val z_index : int -> io -> io
  (** [z_index z io] controls the depth [z] of the following draws. *)

  val color : Color.t -> io -> io
  (** [color c io] specifies the default color [c]. *)

  (** {2 Font} *)

  val font : Font.t -> io -> io
  (** [font f io] specifies the default font [f]. *)

  val font_size : int -> io -> io
  (** [font_size s io] specifies the default font size [s]. *)
end

module Window : sig
  (** Configure the game window. *)

  val show_cursor : io:io -> bool -> unit
  (** [show_cursor ~io visible] toggles the visibility of the operating system mouse cursor. *)

  val set_size : io:io -> Size.t -> unit
  (** [set_size ~io wh] resizes the operating system window.

      Example:

      {[
      Window.set_size ~io (Size.v 800. 800.) ;
      ]}

  *)

  val size : io:io -> Size.t
  (** [size ~io] returns the current size of the game window. *)

  val box : io:io -> Box.t
  (** [box ~io] returns a box matching the game window contents.

      Example:

      {[
      (* clear the screen *)
      Box.fill ~io ~color:Color.white (Window.box ~io) ;
      ]}
  *)
end

(** {1 Animations} *)

val clock : io:io -> float
(** [clock ~io] returns the number of elapsed seconds since the game started until the current frame. The clock is defined at the beginning of a frame, calling [clock] multiple times will always produce the same result.

Examples:

{[
(* circle moving around a circle *)
let center = Vec.(100.0 * v (1. +. cos (clock ~io)) (1. +. sin (clock ~io))) in
Circle.fill ~io (Circle.v center 10.0);
]}

{[
(* show elapsed time since the last click *)
Gamelle.run 0. @@ fun ~io last_click ->
let now = clock ~io in
let last_click = if Input.is_up ~io `click_left then now else last_click in
let elapsed = now -. last_click in
draw_string ~io ~at:Point.zero (Printf.sprintf "Time since clicked: %fs" elapsed);
last_click
]}

*)

val dt : io:io -> float
(** [dt ~io] is the duration of a frame, which is fixed to a 60fps framerate.

Example:

{[
Gamelle.run (Point.v 200. 200., Vec.zero) @@ fun ~io (position, velocity) ->
let acceleration = Vec.v 0. 9.81 in (* gravity *)
let velocity = Vec.(velocity + dt ~io * acceleration) in
let position = Vec.(position + dt ~io * velocity) in
Circle.draw ~io (Circle.v position 20.0);
(position, velocity)
]}

*)

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

  val frames : float -> 'a array -> 'a t
  (** [frames duration arr] is an animation, lasting [duration] seconds, which steps over the values of the array [arr]. *)

  val update : dt:float -> 'a t -> 'a t
  (** [update ~dt t] moves the animation [t] forward in time by [dt] seconds. See {!dt}. *)

  (** {2 Accessors} *)

  val get : 'a t -> 'a
  (** [get t] returns the current ['a] value. *)

  val last : 'a t -> 'a
  (** [last t] returns the last ['a] value, at the end of the animation. *)

  val duration : 'a t -> float
  (** [duration t] is the length of the animation [t]. Returns [0.0] if the animation has completed. *)

  (** {2 Composition} *)

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

  (** {2 Transforms} *)

  val rev : 'a t -> 'a t
  (** [rev t] returns the animation [t] playing backward. *)

  val scale : float -> 'a t -> 'a t
  (** [scale f t] stretches the animation [t] duration by a factor [f]. *)

  val cut : float -> 'a t -> 'a t * 'a t
  (** [cut duration t] returns the animation [t] truncated to [duration], and the rest. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn t] applies [fn] to each output value of the animation [t]. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 fn ta tb] applies [fn] to each animation [ta] and [tb] values. *)
end

module Physics : sig
  (** Rigid physics for {!Shape} objects. *)

  type t
  (** The type of rigid bodies. *)

  type kind = Movable | Immovable

  val v :
    ?mass:float ->
    ?inertia:float ->
    ?restitution:float ->
    ?kind:kind ->
    Shape.t ->
    t
  (** [v s] returns a new rigid body with shape [s].

    - [?mass] is the weight of the rigid body.
    - [?inertia] controls the reactivity of the rigid body.
    - [?restitution] defines the bouncyness of the rigid body.
    - [?kind] controls if the rigid body can move.

  *)

  (** {2 Accessors} *)

  val shape : t -> Shape.t
  (** [shape t] returns the shape of the rigid body [t]. *)

  val center : t -> Point.t
  (** [center t] returns the center of mass of the shape [t]. *)

  val velocity : t -> Vec.t
  (** [velocity t] returns the current velocity of the rigid body [t]. *)

  val rotation : t -> float
  (** [center t] returns the rotation of the rigid body [t]. *)

  val rot_velocity : t -> float
  (** [rot_velocity t] returns the current rotational velocity of the rigid body [t]. *)

  (** {2 Simulation} *)

  val add_velocity : Vec.t -> t -> t
  (** [add_velocity v t] adds [v] to the rigid body [t] current {!velocity}. *)

  val add_rot_velocity : float -> t -> t
  (** [add_rot_velocity r t] adds [r] to the rigid body [t] current rotational velocity. *)

  val update : dt:float -> t -> t
  (** [update ~dt t] updates the rigid body [t] position and rotation according to its current velocities and delta time [dt]. *)

  val fix_collisions : t list -> t list
  (** [fix_collisions lst] detects and repairs any collisions between the rigid bodies in the list [lst]. *)

  (** {2 Teleportation} *)

  val set_center : Point.t -> t -> t
  (** [set_center pt t] teleports the rigid body [t] to have a center at point [pt]. *)

  val set_rotation : float -> t -> t
  (** [set_rotation r t] rotates the rigid body [t] to have a rotation [r]. *)

  val set_velocity : Vec.t -> t -> t
  (** [set_velocity speed t] sets the velocity of the rigid body [t] to the vector [speed]. *)

  val set_rot_velocity : float -> t -> t
  (** [set_rot_velocity dr t] sets the rotational velocity of the rigid body [t] to [dr]. *)

  (** {2 Draw} *)

  val draw : io:io -> ?color:Color.t -> t -> unit
  (** [draw ~io t] draws the outline border of the rigid body [t]. *)

  val fill : io:io -> ?color:Color.t -> t -> unit
  (** [fill ~io t] fills the inside of the rigid body [t]. *)
end
