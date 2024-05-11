open Draw_geometry

type t

val v :
  ?min_width:float ->
  ?flex_width:float ->
  ?min_height:float ->
  ?flex_height:float ->
  (Box.t -> unit) ->
  t

val fixed : ?width:float -> ?height:float -> (Box.t -> unit) -> t

type h

val height : ?min:float -> ?flex:float -> (Box.t -> unit) -> h
val width : ?min:float -> ?flex:float -> (float -> h) -> t

(* *)

val horizontal : ?gap:float -> t list -> t
val vertical : ?gap:float -> t list -> t
val over : t list -> t
val vclip : float -> t -> t
val padded : float -> t list -> t
val center : t list -> t
val solve : ?width:(float -> float) -> ?height:(float -> float) -> t -> unit
