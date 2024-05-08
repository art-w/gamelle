(* open Gamelle_common.Geometry *)

module type S = sig
  type io
  type ui

  val update_loc : ui -> string -> ui

  type point
  type box
  type size

  module Style : sig
    type alignment = Start | End | Center | Fill
    type t = { growth : float; vertical : alignment; horizontal : alignment }

    val default : t
    val growth : float -> t
    val vertical : alignment -> t
    val horizontal : alignment -> t
    val ( & ) : t -> t -> t
  end

  val window : ?debug:bool -> io:io -> point -> (ui -> 'a) -> 'a * box
  val button : ui -> ?id:int -> ?init:bool -> ?style:Style.t -> string -> bool
  val checkbox : ui -> ?id:int -> ?init:bool -> ?style:Style.t -> string -> bool
  val label : ui -> ?style:Style.t -> string -> unit
  val text_area : ui -> ?style:Style.t -> ?width:float -> string -> unit

  val slider :
    ?id:int ->
    ?init:float ->
    ?style:Style.t ->
    ?width:float ->
    ui ->
    min:float ->
    max:float ->
    float

  val int_slider :
    ?id:int ->
    ?init:int ->
    ?style:Style.t ->
    ?width:float ->
    ui ->
    min:int ->
    max:int ->
    int

  val vertical : ui -> ?style:Style.t -> (unit -> 'a) -> 'a
  val horizontal : ui -> ?style:Style.t -> (unit -> 'a) -> 'a
  val vscroll : ui -> ?style:Style.t -> height:float -> (unit -> 'a) -> 'a

  val radio :
    ui ->
    ?id:int ->
    ?init:'a option ->
    ?style:Style.t ->
    ('a * string) list ->
    'a option

  val text_input :
    ui -> ?id:int -> ?init:string -> ?style:Style.t -> float -> string

  val nest_loc : ui -> (unit -> 'a) -> 'a

  module type Widget = sig
    type params
    type state
    type return

    val size : ts:(string -> size) -> params -> size
    val render : io:io -> params -> state -> box -> unit
    val update : io:io -> params -> state -> box -> state
    val result : params -> state -> return

    val v :
      ui ->
      ?id:int ->
      ?init:return ->
      ?size:(ts:(string -> size) -> params -> size) ->
      ?style:Style.t ->
      ?render:(io:io -> params -> state -> box -> unit) ->
      params ->
      return
  end

  module Customize : sig
    module Button :
      Widget
        with type params = string
         and type state = bool
         and type return = bool

    module Checkbox :
      Widget
        with type params = string
         and type state = bool
         and type return = bool

    module Horizontal = Horizontal

    (* module Label : Inert_widget with type params = string *)
    module Radio = Radio
    module Slider = Slider
    module Int_slider = Int_slider
    module Text_area = Text_area
    module Text_input = Text_input
    module Vertical = Vertical
    module Widget_builder = Widget_builder
  end
end
