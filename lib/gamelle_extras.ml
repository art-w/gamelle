open Gamelle_backend
open Gamelle_common
open Geometry
let text_area_size ~io ?(width = Float.infinity) ?(interline = -8.) ?font ?size
    text =
  let pos = V2.zero in
  let text_size = text_size ~io ?font ?size in
  let limx = width +. P2.x pos in
  let startx = P2.x pos in
  let lines = String.split_on_char '\n' text in
  let hline = Size2.h (text_size "a") +. interline in
  let udpate_cpos maxw cpos = (Float.max maxw (Size2.w cpos), cpos) in
  let print_line (maxw, cpos) line =
    let words = String.split_on_char ' ' line in
    let print_word (maxw, cpos) word =
      let size = text_size (word ^ " ") in
      let w = Size2.w size in
      let split_word word cpos =
        let chars =
          word |> String.to_seq |> Seq.map (fun c -> String.init 1 (fun _ -> c))
        in
        Seq.fold_left
          (fun (maxw, cpos) char ->
            let size = text_size char in
            let w = Size2.w size in
            let cpos =
              if w +. P2.x cpos >= limx then P2.v startx (P2.y cpos +. hline)
              else cpos
            in
            udpate_cpos maxw V2.(cpos + v w 0.))
          (maxw, cpos) chars
      in
      let maxw, cpos =
        if w >= width then split_word word cpos
        else if w +. P2.x cpos >= limx then
          let cpos = P2.v startx (P2.y cpos +. hline) in
          (maxw, cpos)
        else (maxw, cpos)
      in
      udpate_cpos maxw V2.(cpos + v w 0.)
    in

    let maxw, cpos = List.fold_left print_word (maxw, cpos) words in
    (maxw, P2.v startx (P2.y cpos +. hline))
  in
  let maxw, end_pos = List.fold_left print_line (0., pos) lines in
  Size2.v maxw (P2.y end_pos)

let draw_text ~io ?(width = Float.infinity) ?(interline = -8.) ?font ~color
    ?size text pos =
  let text_size = text_size ~io ?font ?size in
  let draw_string = draw_string ~io ~color ?size ?font in
  let limx = width +. P2.x pos in
  let startx = P2.x pos in
  let lines = String.split_on_char '\n' text in
  let hline = Size2.h (text_size "a") +. interline in
  let print_line cpos line =
    let words = String.split_on_char ' ' line in
    let print_word cpos word =
      let size = text_size (word ^ " ") in
      let w = Size2.w size in
      let split_word word cpos =
        let chars =
          word |> String.to_seq |> Seq.map (fun c -> String.init 1 (fun _ -> c))
        in
        Seq.fold_left
          (fun cpos char ->
            let size = text_size char in
            let w = Size2.w size in
            let cpos =
              if w +. P2.x cpos >= limx then P2.v startx (P2.y cpos +. hline)
              else cpos
            in
            draw_string char cpos;
            V2.(cpos + v w 0.))
          cpos chars
      in
      let cpos =
        if w >= width then split_word word cpos
        else if w +. P2.x cpos >= limx then (
          let cpos = P2.v startx (P2.y cpos +. hline) in
          draw_string word cpos;
          cpos)
        else (
          draw_string (word ^ " ") cpos;
          cpos)
      in
      V2.(cpos + v w 0.)
    in
    let cpos = List.fold_left print_word cpos words in
    P2.v startx (P2.y cpos +. hline)
  in
  ignore (List.fold_left print_line pos lines);
  ()

let milliseconds () = int_of_float (Sys.time () *. 10000.)
let rythm n = milliseconds () mod n = 0
