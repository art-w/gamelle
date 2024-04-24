open Gamelle_common
open Gamelle_backend

module Point = struct
  include Geometry.Point
end

module Vec = struct
  include Geometry.Vec
end

module Color = struct
  include Geometry.Color
end

module Segment = struct
  include Geometry.Segment

  let draw = draw_line
end

module Circle = struct
  include Geometry.Circle

  let draw = draw_circle
  let fill = fill_circle
end

module Box = struct
  include Geometry.Box

  let draw = draw_rect
  let fill = fill_rect
end

module Size = Geometry.Size
module Size1 = Geometry.Size1

type color = Geometry.color
type point = Geometry.point
type vec = Geometry.vec
type segment = Geometry.segment
type circle = Geometry.circle
type box = Geometry.box
type size = Geometry.size

module Text = struct
  include Gamelle_backend.Text

  let sub txt i l = slice ~start:i ~stop:(i + l) txt

  let split_on_char char t =
    t |> to_string |> String.split_on_char char |> List.map of_string

  let concat li =
    let buffer = Buffer.create 128 in
    List.iter (fun str -> Buffer.add_string buffer (to_string str)) li;
    of_string (Buffer.contents buffer)

  let draw_t = draw

  let draw ~io ~color ?font ?size t pos =
    draw_t ~io ~color ?font ?size (of_string t) pos

  let size_t = size
  let size ~io ?font ?size t = size_t ~io ?font ?size (of_string t)

  let size_multiline_t ~io ?(width = Float.infinity) ?(interline = -8.) ?font
      ?size (text : t) =
    let pos = Vec.zero in
    let text_size = Gamelle_backend.Text.size ~io ?font ?size in
    let limx = width +. Point.x pos in
    let startx = Point.x pos in
    let lines = split_on_char '\n' text in
    let hline = Size.h (text_size (Text.of_string "a")) +. interline in
    let udpate_cpos maxw cpos = (Float.max maxw (Size.w cpos), cpos) in
    let print_line (maxw, cpos) line =
      let words = split_on_char ' ' line in
      let print_word (maxw, cpos) word =
        let size = text_size (word ^ of_string " ") in
        let w = Size.w size in
        let split_word word cpos =
          let chars = chars word in
          List.fold_left
            (fun (maxw, cpos) char ->
              let size = text_size char in
              let w = Size.w size in
              let cpos =
                if w +. Point.x cpos >= limx then
                  Point.v startx (Point.y cpos +. hline)
                else cpos
              in
              udpate_cpos maxw Vec.(cpos + v w 0.))
            (maxw, cpos) chars
        in
        let maxw, cpos =
          if w >= width then split_word word cpos
          else if w +. Point.x cpos >= limx then
            let cpos = Point.v startx (Point.y cpos +. hline) in
            (maxw, cpos)
          else (maxw, cpos)
        in
        udpate_cpos maxw Vec.(cpos + v w 0.)
      in

      let maxw, cpos = List.fold_left print_word (maxw, cpos) words in
      (maxw, Point.v startx (Point.y cpos +. hline))
    in
    let maxw, end_pos = List.fold_left print_line (0., pos) lines in
    Size.v maxw (Point.y end_pos)

  let size_multiline ~io ?width ?interline ?font ?size str =
    size_multiline_t ~io ?width ?interline ?font ?size (of_string str)

  let draw_multiline_t ~io ?(width = Float.infinity) ?(interline = -8.) ?font
      ~color ?size text pos =
    let text_size = Gamelle_backend.Text.size ~io ?font ?size in
    let draw_string = draw_t ~io ~color ?size ?font in
    let limx = width +. Point.x pos in
    let startx = Point.x pos in
    let lines = split_on_char '\n' text in
    let hline = Size.h (text_size (of_string "a")) +. interline in
    let print_line cpos line =
      let words = split_on_char ' ' line in
      let print_word cpos word =
        let size = text_size (word ^ of_string " ") in
        let w = Size.w size in
        let split_word word cpos =
          let chars = chars word in
          List.fold_left
            (fun cpos char ->
              let size = text_size char in
              let w = Size.w size in
              let cpos =
                if w +. Point.x cpos >= limx then
                  Point.v startx (Point.y cpos +. hline)
                else cpos
              in
              draw_string char cpos;
              Vec.(cpos + v w 0.))
            cpos chars
        in
        let cpos =
          if w >= width then split_word word cpos
          else if w +. Point.x cpos >= limx then (
            let cpos = Point.v startx (Point.y cpos +. hline) in
            draw_string word cpos;
            cpos)
          else (
            draw_string (word ^ of_string " ") cpos;
            cpos)
        in
        Vec.(cpos + v w 0.)
      in
      let cpos = List.fold_left print_word cpos words in
      Point.v startx (Point.y cpos +. hline)
    in
    ignore (List.fold_left print_line pos lines);
    ()

  let draw_multiline ~io ?width ?interline ?font ~color ?size str pos =
    draw_multiline_t ~io ?width ?interline ?font ~color ?size (of_string str)
      pos
end
