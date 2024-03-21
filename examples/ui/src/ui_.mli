open Gamelle
open Geometry

type t
type id = int

type ('state, 'params, 'r) elt =
  ui:t ->
  id:id ->
  ?size:(io:io -> 'params -> size2) ->
  ?render:(io:io -> 'params -> 'state -> box2 -> unit) ->
  ?update:(io:io -> box2 -> 'state -> 'params -> 'state) ->
  ?result:(ui:t -> 'state -> box2 -> 'r) ->
  'params ->
  'r

type size_requirement = { available : size1; minimal : size1 }
(** one dim because this is orthogonal to direction *)

type ('state, 'params, 'r) node =
  ui:t ->
  id:id ->
  ?size:(io:io -> req:size_requirement -> 'params -> size2) ->
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

val ui : ?debug:bool -> io:io -> id:id -> p2 -> (t -> 'a) -> 'a * box2
val button : (unit, string, bool) elt
val checkbox : (bool, string, bool) elt
val label : ui:t -> id:id -> string -> unit
val slider : (slider_state, slider_params, float) elt
val vertical : ui:t -> (unit -> 'a) -> 'a
val horizontal : ui:t -> (unit -> 'a) -> 'a
val scroll_box : (scroll_box_state, 'a scroll_box_params, 'a) node
