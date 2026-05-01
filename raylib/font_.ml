open Common
open Gamelle_common
open Geometry
module Delayed = Gamelle_common.Delayed

let get_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  to_raylib_color c

type t = font

let set_font font io = { io with font }
let set_font_size font_size io = { io with font_size }

let load binstring =
  Delayed.make @@ fun ~io:_ -> { data = binstring; sizes = Hashtbl.create 16 }

let default : t = load Gamelle_common.Font.default
let default_size = Gamelle_common.Font.default_size

(* Initial codepoint range loaded eagerly: ASCII + Latin extended + Greek + Cyrillic. *)
let initial_codepoints =
  Array.init (1103 - 32 + 1) (fun i -> 32 + i)

let load_font_with_codepoints data size codepoints =
  let n = Array.length codepoints in
  let arr = Ctypes.CArray.make Ctypes.int n in
  Array.iteri (fun i cp -> Ctypes.CArray.set arr i cp) codepoints;
  let f =
    Raylib.load_font_from_memory ".ttf" data (String.length data) size
      (Ctypes.CArray.start arr) n
  in
  assert (Raylib.is_font_valid f);
  f

let iter_codepoints text f =
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    let d = String.get_utf_8_uchar text !i in
    f (Uchar.to_int (Uchar.utf_decode_uchar d));
    i := !i + Uchar.utf_decode_length d
  done

(* Reload the font if any codepoint in [text] is not yet in the atlas. *)
let ensure_codepoints font_s sf size text =
  let missing = ref [] in
  iter_codepoints text (fun cp ->
      if not (Hashtbl.mem sf.codepoint_set cp) then missing := cp :: !missing);
  if !missing <> [] then begin
    List.iter (fun cp -> Hashtbl.replace sf.codepoint_set cp ()) !missing;
    let all_cps =
      Array.of_list
        (Hashtbl.fold (fun cp () acc -> cp :: acc) sf.codepoint_set [])
    in
    Raylib.unload_font sf.raylib_font;
    sf.raylib_font <- load_font_with_codepoints font_s.data size all_cps
  end

let get_sized_font ~io font size =
  let font_s = Delayed.force ~io font in
  match Hashtbl.find_opt font_s.sizes size with
  | Some sf -> (font_s, sf)
  | None ->
      let cp_set = Hashtbl.create 2048 in
      Array.iter (fun cp -> Hashtbl.replace cp_set cp ()) initial_codepoints;
      let raylib_font =
        load_font_with_codepoints font_s.data size initial_codepoints
      in
      let sf = { raylib_font; codepoint_set = cp_set } in
      clean_io ~io (fun () ->
          Hashtbl.remove font_s.sizes size;
          Raylib.unload_font sf.raylib_font);
      Hashtbl.replace font_s.sizes size sf;
      (font_s, sf)

let get ~io font_opt size_opt text =
  let font, size = get_font ~io font_opt size_opt in
  (* multiply by a magic constant to get close to the same size as the JS backend. *)
  let size = int_of_float (float_of_int size *. 26. /. 18.) in
  let font_s, sf = get_sized_font ~io font size in
  ensure_codepoints font_s sf size text;
  (sf.raylib_font, float_of_int size)

let text_size ~io ?font ?size text =
  let raylib_font, font_size = get ~io font size text in
  let v = Raylib.measure_text_ex raylib_font text font_size 0. in
  Size.v (Raylib.Vector2.x v) (Raylib.Vector2.y v)

let tau = 8.0 *. atan 1.0

let draw_text ~io ?color ?font ?size ~at:p text =
  let raylib_font, font_size = get ~io font size text in
  let color = get_color ~io color in
  let x, y = project ~io p in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  with_scissor ~io begin fun () ->
      Raylib.draw_text_pro raylib_font text
        (Raylib.Vector2.create x y)
        (Raylib.Vector2.create 0. 0.)
        angle font_size 0. color
    end
