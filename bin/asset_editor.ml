open Gamelle

type pos = float * float
type state = Bitmap.t * pos option * (pos * pos) list

let update ev ((bmp, fst_pos, rects) as st) =
  match (fst_pos, Event.is_pressed ev Click_left) with
  | Some _, true | None, false -> st
  | None, true ->
      let pos = Event.mouse_pos ev in
      (bmp, Some pos, rects)
  | Some fst_pos, false ->
      let pos = Event.mouse_pos ev in
      (bmp, None, (fst_pos, pos) :: rects)

let render ~view (bmp, pos, rects) =
  fill_rect ~view ~color:Color.black (0., 0.) (400., 400.);
  show_cursor true;
  draw ~view bmp 0. 0.;
  Option.iter (fun pos -> fill_circle ~view ~color:Color.green pos 1.) pos;
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      (* some strong assumptions here *)
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      draw_rect ~view ~color:Color.blue (x1, y1) (w, h))
    rects

let print_rects ~file (_, _, rects) =
  let oc = open_out file in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      (* some strong assumptions here *)
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      Printf.fprintf oc "%.0f %.0f %.0f %.0f\n%!" x1 y1 w h)
    rects;
  close_out oc

let do_the_thing out file =
  let bmp = Bitmap.load file in
  let on_exit = print_rects ~file:out in
  run (bmp, None, []) ~update ~render ~on_exit
