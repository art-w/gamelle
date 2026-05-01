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

let get_sized_font ~io font size =
  let font_s = Delayed.force ~io font in
  match Hashtbl.find font_s.sizes size with
  | f -> f
  | exception Not_found ->
      (* Load codepoints 32–1103: covers ASCII, Latin extended, Greek, Cyrillic.
         Passing null/0 would only load ASCII 32–127. *)
      let first_cp = 32 and last_cp = 1103 in
      let n = last_cp - first_cp + 1 in
      let arr = Ctypes.CArray.make Ctypes.int n in
      for i = 0 to n - 1 do
        Ctypes.CArray.set arr i (first_cp + i)
      done;
      let f =
        Raylib.load_font_from_memory ".ttf" font_s.data
          (String.length font_s.data)
          size (Ctypes.CArray.start arr) n
      in
      assert (Raylib.is_font_valid f);
      clean_io ~io (fun () ->
          Hashtbl.remove font_s.sizes size;
          Raylib.unload_font f);
      Hashtbl.replace font_s.sizes size f;
      f

let get ~io font_opt size_opt =
  let font, size = get_font ~io font_opt size_opt in
  (* multiply by a magic constant to get close to the same size as the JS backend. *)
  let size = int_of_float (float_of_int size *. 26. /. 18.) in
  (get_sized_font ~io font size, float_of_int size)

let text_size ~io ?font ?size text =
  let raylib_font, font_size = get ~io font size in

  let v = Raylib.measure_text_ex raylib_font text font_size 0. in
  Size.v (Raylib.Vector2.x v) (Raylib.Vector2.y v)

let tau = 8.0 *. atan 1.0

let draw_text ~io ?color ?font ?size ~at:p text =
  let raylib_font, font_size = get ~io font size in

  let color = get_color ~io color in
  let x, y = project ~io p in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  with_scissor ~io begin fun () ->
      Raylib.draw_text_pro raylib_font text
        (Raylib.Vector2.create x y)
        (Raylib.Vector2.create 0. 0.)
        angle font_size 0. color
    end
