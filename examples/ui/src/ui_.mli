open Gamelle
open Geometry

type t

type ('state, 'params, 'r) elt =
  ui:t ->
  ?id:int ->
  ?size:(io:io -> space_available:size1 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> 'params -> 'state -> box2 -> 'state) ->
  ?result:('state -> 'r) ->
  'params ->
  'r

type ('state, 'params, 'r) node =
  ui:t ->
  ?id:int ->
  ?size:
    (io:io -> space_available:size1 -> space_required:size1 -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> children_size:size2 -> box2 -> 'state -> 'params -> 'state) ->
  ?result:('params -> 'r) ->
  'params ->
  'r

type scroll_box_state = {
  size : size2;
  offset : float;
  grasped : bool;
  real_height : float;
}

type slider_state = { v : float; grasped : bool }
type slider_params = { w : float; min : float; max : float }
type 'a scroll_box_params = { height : float; f : unit -> 'a }

val ui : ?debug:bool -> io:io -> p2 -> (t -> 'a) -> 'a * box2
val button : (bool, string, bool) elt
val checkbox : (bool, string, bool) elt
val label : ui:t -> ?id:int -> string -> unit
val slider : (slider_state, slider_params, float) elt
val vertical : ui:t -> ?id:int -> (unit -> 'a) -> 'a
val horizontal : ui:t -> ?id:int -> (unit -> 'a) -> 'a
val scroll_box : (scroll_box_state, 'a scroll_box_params, 'a) node
