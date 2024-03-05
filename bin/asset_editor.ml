open Gamelle
open Gg

type pos = float * float

type state = {
  bmp : Bitmap.t;
  scale : float;
  mouse : pos;
  click : pos option;
  rects : (pos * pos) list;
}

let fresh_state file =
  {
    bmp = Bitmap.load file;
    scale = 1.;
    mouse = (0., 0.);
    click = None;
    rects = [];
  }

let compute_rect ((x1, y1), (x2, y2)) =
  (* some strong assumptions here *)
  let w = x2 -. x1 in
  let h = y2 -. y1 in
  Box2.v (P2.v x1 y1) (Size2.v w h)

let main ~view ev st =
  let x, y = Event.mouse_pos ev in
  let mouse = (x /. st.scale, y /. st.scale) in
  let st = { st with mouse } in
  let st =
    match (st.click, Event.is_pressed ev Click_left) with
    | None, false ->
        if Event.is_pressed ev Wheel_up then { st with scale = st.scale +. 1. }
        else if Event.is_pressed ev Wheel_down then
          { st with scale = st.scale -. 1. }
        else st
    | Some _, true -> st (* just keep dragging *)
    | None, true -> { st with click = Some st.mouse }
    | Some fst_pos, false ->
        { st with click = None; rects = (fst_pos, st.mouse) :: st.rects }
  in
  fill_rect ~view ~color:Color.black (Box2.v (P2.v 0. 0.) (P2.v 500. 500.));
  show_cursor true;
  let view = View.scaled st.scale view in
  draw ~view st.bmp (P2.v 0. 0.);
  Option.iter
    (fun pos ->
      let rect = compute_rect (pos, st.mouse) in
      draw_rect ~view ~color:Color.green rect)
    st.click;
  let pale_green = Color.(with_a green 0.2) in
  List.iter
    (fun poss ->
      let rect = compute_rect poss in
      fill_rect ~view ~color:pale_green rect)
    st.rects;
  st

let print_rects ~file { rects; _ } =
  let oc = open_out file in
  List.iter
    (fun poss ->
      let rect = compute_rect poss in
      let x, y = V2.to_tuple (Box2.o rect) in
      let w, h = V2.to_tuple (Box2.size rect) in
      Printf.fprintf oc "%.0f %.0f %.0f %.0f\n%!" x y w h)
    rects;
  close_out oc

let do_the_thing out file =
  let st = fresh_state file in
  let on_exit = print_rects ~file:out in
  run st ~on_exit main
