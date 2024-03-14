open Gamelle
open Geometry

module Ui : sig
  type t
  type id = int

  val ui : io:io -> id:id -> p2 -> (t -> 'a) -> 'a * box2
  val button : ui:t -> string -> bool
  val checkbox : ui:t -> id:id -> string -> bool
  val label : ui:t -> string -> unit
end = struct
  module IntMap = Map.Make (Int)

  type id = int
  type t = { io : io; id : id; mutable pos : p2; mutable max_width : size1 }

  let state = Hashtbl.create 256
  let padding = 6.
  let padding_x = V2.v padding 0.
  let padding_y = V2.v 0. padding
  let padding_vec = V2.(padding_x + padding_y)
  let fg = Color.Gruvbox.Dark.fg
  let bg = Color.Gruvbox.Dark.bg
  let highlight = Color.Gruvbox.Dark.blue

  let update_max_width ~ui candidate =
    ui.max_width <- Float.max ui.max_width candidate

  let ui ~io ~id pos f =
    let ctx = { io; id; pos = V2.(pos + padding_x); max_width = 0. } in
    if Option.is_none (Hashtbl.find_opt state id) then
      Hashtbl.add state id (Hashtbl.create 256);
    let r = f ctx in
    let end_corner =
      P2.(v (ctx.max_width +. (2. *. padding)) (y ctx.pos +. padding))
    in
    let box = Box.v_corners pos end_corner in
    draw_rect ~io ~color:fg box;
    (r, box)

  let is_clicked ~io box =
    Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

  let button ~ui text =
    let { io; pos; _ } = ui in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    let pos = V2.(pos + padding_y) in
    let box = Box.v pos V2.(text_size + (2. * (padding_x + padding_y))) in
    let pos = V2.(pos + padding_y) in
    update_max_width ~ui (Box.w box);
    fill_rect ~io ~color:bg box;
    draw_rect ~io ~color:fg box;
    draw_string ~io ~color:fg Font.default ~size text V2.(pos + padding_x);
    let pos = P2.(v (x pos) (Box.maxy box)) in
    ui.pos <- pos;
    is_clicked ~io box

  let is_checked ~ui ~id box =
    let { id = id_ui; io; _ } = ui in
    let ui_state = Hashtbl.find state id_ui in
    let is_clicked = is_clicked ~io box in
    let state_b =
      match Hashtbl.find_opt ui_state id with
      | None ->
          Hashtbl.add ui_state id false;
          false
      | Some b -> b
    in
    Hashtbl.replace state id_ui ui_state;
    let r = if is_clicked then not state_b else state_b in
    Hashtbl.replace ui_state id r;
    r

  let checkbox ~ui ~id text =
    let { io; id = _; pos; max_width = _ } = ui in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    let check'box'_size = Size2.h text_size in

    let pos = V2.(pos + padding_y) in
    let box =
      Box.v pos
        V2.(
          text_size
          + (2. * (padding_x + padding_y))
          + Size2.v (check'box'_size +. padding) 0.)
    in
    let pos = V2.(pos + padding_y) in
    let check'box' =
      Box.(
        v
          V2.(o box + padding_x + padding_y)
          Size2.(v check'box'_size check'box'_size))
    in
    let is_checked = is_checked ~ui ~id box in
    fill_rect ~io ~color:bg box;
    draw_rect ~io ~color:fg box;
    fill_rect ~io ~color:fg check'box';
    (if is_checked then
       let ticked'box' =
         Box.(
           v
             V2.(o check'box' + padding_x + padding_y)
             V2.(Box.size check'box' - (2. * padding_vec)))
       in
       fill_rect ~io ~color:highlight ticked'box');
    draw_string ~io ~color:fg Font.default ~size text
      V2.(pos + padding_x + v check'box'_size 0. + padding_x);
    update_max_width ~ui (Box.w box);
    let pos = P2.(v (x pos) (Box.maxy box)) in
    ui.pos <- pos;
    is_checked

  let label ~ui text =
    let { io; pos; _ } = ui in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    update_max_width ~ui (Size2.w text_size);
    draw_string ~io ~color:fg Font.default ~size text pos;
    let pos = P2.(v (x pos) (y pos +. y text_size)) in
    ui.pos <- pos
end

type state = unit

let () =
  Gamelle.run Box.zero @@ fun ~io box ->
  if Event.is_pressed ~io `escape then raise Exit;
  show_cursor true;
  let io = View.drawing_box box io in
  snd
    Ui.(
      ui ~io ~id:0
        P2.(v 0. 0.)
        (fun ui ->
          label ~ui "This is a label";
          if button ~ui "This is a button" then print_endline "button pressed";
          ignore @@ checkbox ~ui ~id:1 "This is a checkbox 🤓"))
