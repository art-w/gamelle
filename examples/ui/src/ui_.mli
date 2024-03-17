open Gamelle
open Geometry

type t
type id = int

val ui : io:io -> id:id -> p2 -> (t -> 'a) -> 'a * box2
val button : ui:t -> string -> bool
val checkbox : ui:t -> id:id -> string -> bool
val label : ui:t -> string -> unit
val slider : ui:t -> id:id -> w:size1 -> min:float -> max:float -> float
