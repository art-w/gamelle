open Gamelle_common
open Geometry

type t
type cap = t * string
type alignment = Start | End | Center | Fill
type style = { vertical : alignment; horizontal : alignment }

type ('state, 'params, 'r) elt =
  t * string ->
  ?id:int ->
  ?size:(ts:(string -> size) -> 'params -> size) ->
  ?weight:float ->
  ?style:style ->
  ?render:(io:io -> 'params -> 'state -> box -> unit) ->
  'params ->
  'r

type 'params inert_elt =
  t * string ->
  ?id:int ->
  ?style:style ->
  ?size:(ts:(string -> size) -> 'params -> size) ->
  ?render:(io:io -> 'params -> box -> unit) ->
  'params ->
  unit

type ('state, 'params, 'r) node =
  t * string ->
  ?id:int ->
  ?style:style ->
  ?size:(ts:(string -> size) -> children_size:size -> 'params -> size) ->
  ?weight:float ->
  ?render:(io:io -> 'params -> 'state -> box -> unit) ->
  'params ->
  'r

type vscroll_state = {
  size : size;
  offset : float;
  grasped : bool;
  real_height : float;
}

type slider_state = { v : float; grasped : bool }
type slider_params = { w : float; min : float; max : float }
type 'a vscroll_params = { height : float; f : unit -> 'a }

val window : ?debug:bool -> io:io -> point -> (t -> 'a) -> 'a * box
val button : (bool, string, bool) elt
val checkbox : (bool, string, bool) elt
val label : cap -> ?style:style -> ?weight:float -> string -> unit

val text_area :
  cap -> ?style:style -> ?weight:float -> ?width:float -> string -> unit

val slider : (slider_state, slider_params, float) elt
val vertical : cap -> ?weight:float -> (unit -> 'a) -> 'a
val horizontal : cap -> ?weight:float -> (unit -> 'a) -> 'a
val vscroll : (vscroll_state, 'a vscroll_params, 'a) node
val radio : (Radio.state, 'a Radio.params, 'a Radio.return) elt
val text_input : (Text_input.state, float, string) elt
val nest_loc : cap -> (unit -> 'a) -> 'a
