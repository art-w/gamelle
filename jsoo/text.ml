include Jstr
open Draw
open Gamelle_common
open Geometry

let ( ^ ) = append

let draw ~io ~color ?(font = Font_.default) ?(size = Font.default_size) txt p =
  transform ~io;
  set_color ~io color;
  let x, y = Vec.to_tuple p in
  Font_.draw_at ~io font ~size (to_string txt) (x, y)

let size ~io ?(font = Font_.default) ?(size = Font.default_size) txt =
  Font_.text_size ~io font ~size (to_string txt)

let get = get_jstr
let chars txt = List.init (length txt) (fun i -> get txt i)
