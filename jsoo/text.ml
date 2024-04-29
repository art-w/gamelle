include Jstr
open Draw
open Gamelle_common

let ( ^ ) = append

let draw ~io ?color ?(font = Font_.default) ?(size = Font.default_size) ~at txt
    =
  transform ~io;
  set_color ~io color;
  Font_.draw_at ~io ~font ~size (Jstr.to_string txt) ~at

let size ~io ?(font = Font_.default) ?(size = Font.default_size) txt =
  Font_.text_size ~io ~font ~size (Jstr.to_string txt)

let get = get_jstr
let chars txt = List.init (length txt) (fun i -> get txt i)
