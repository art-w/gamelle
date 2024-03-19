open Gamelle
open Geometry

type t
type id = int

val ui : ?debug:bool -> io:io -> id:id -> p2 -> (t -> 'a) -> 'a * box2
val button : ui:t -> string -> bool
val checkbox : ui:t -> id:id -> string -> bool
val label : ui:t -> string -> unit
val slider : ui:t -> id:id -> w:size1 -> min:float -> max:float -> float
val vertical : ui:t -> (unit -> 'a) -> 'a
val horizontal : ui:t -> (unit -> 'a) -> 'a
val scroll_box : ui:t -> id:id -> size:size1 -> (unit -> 'a) -> 'a