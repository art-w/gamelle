(* https://github.com/fccm/tsdl-gfx *)

module Gfx : sig
  (** SDL2_gfx bindings for use with Tsdl

      {{:https://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/}SDL2_gfx
       Homepage}

      {{:https://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/_s_d_l2__gfx_primitives_8h.html}SDL2_gfx
       API} *)

  type 'a result = 'a Tsdl.Sdl.result

  val pixel_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val hline_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    x2:int ->
    y:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val vline_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y1:int ->
    y2:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val rectangle_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val rounded_rectangle_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    rad:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val box_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val rounded_box_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    rad:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val line_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val aaline_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val thick_line_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    width:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val circle_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val aacircle_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val filled_circle_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val ellipse_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rx:int ->
    ry:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val aaellipse_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rx:int ->
    ry:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val filled_ellipse_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rx:int ->
    ry:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val arc_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    start:int ->
    end_:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val pie_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    start:int ->
    end_:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val filled_pie_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    rad:int ->
    start:int ->
    end_:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val trigon_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    x3:int ->
    y3:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val aatrigon_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    x3:int ->
    y3:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val filled_trigon_rgba :
    Tsdl.Sdl.renderer ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    x3:int ->
    y3:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val polygon_rgba :
    Tsdl.Sdl.renderer ->
    ps:(int * int) list ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val aapolygon_rgba :
    Tsdl.Sdl.renderer ->
    ps:(int * int) list ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val filled_polygon_rgba :
    Tsdl.Sdl.renderer ->
    ps:(int * int) list ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val bezier_rgba :
    Tsdl.Sdl.renderer ->
    ps:(int * int) list ->
    s:int ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val character_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    c:char ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val string_rgba :
    Tsdl.Sdl.renderer ->
    x:int ->
    y:int ->
    s:string ->
    r:int ->
    g:int ->
    b:int ->
    a:int ->
    unit result

  val set_font_rotation : rot:int -> unit
end
