open Gamelle
open Geometry

module Ui : sig
  type t
  type id = int

  val ui : io:io -> id:id -> p2 -> (t -> 'a) -> 'a
  val button : ui:t -> string -> bool
  val checkbox : ui:t -> id:id -> string -> bool
  val label : ui:t -> string -> unit
end = struct
  module IntMap = Map.Make (Int)

  type id = int
  type t = { io : io; id : id; mutable pos : p2 }

  let state = Hashtbl.create 256

  let ui ~io ~id pos f =
    let ctx = { io; id; pos } in
    if Option.is_none (Hashtbl.find_opt state id) then
      Hashtbl.add state id (Hashtbl.create 256);
    let r = f ctx in
    draw_rect ~io ~color:Color.blue (Box.v_corners pos ctx.pos);
    r

  let is_clicked ~io box =
    Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) box

  let button ~ui text =
    let { io; pos; _ } = ui in
    let color = Color.white in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    let pos = P2.(v (x pos) (y pos +. y text_size)) in
    let box = Box.v pos text_size in
    ui.pos <- pos;
    draw_rect ~io ~color box;
    draw_string ~io ~color Font.default ~size text pos;
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
    let { io; id = _; pos } = ui in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    let pos = P2.(v (x pos) (y pos +. y text_size)) in
    let box = Box.v pos text_size in
    let is_checked = is_checked ~ui ~id box in
    let color = if is_checked then Color.red else Color.white in
    ui.pos <- pos;
    draw_rect ~io ~color box;
    draw_string ~io ~color Font.default ~size text pos;
    is_checked

  let label ~ui text =
    let { io; pos; _ } = ui in
    let color = Color.white in
    let size = 20 in
    let text_size = text_size ~io Font.default ~size text in
    let pos = P2.(v (x pos) (y pos +. y text_size)) in
    ui.pos <- pos;
    draw_string ~io ~color Font.default ~size text pos
end

type state = unit

let () =
  Gamelle.run () @@ fun ~io () ->
  if Event.is_pressed ~io `escape then raise Exit;
  show_cursor true;
  Ui.(
    ui ~io ~id:0
      P2.(v 0. 0.)
      (fun ui ->
        label ~ui "This is a label";
        if button ~ui "This is a button" then print_endline "button pressed";
        ignore @@ checkbox ~ui ~id:1 "This is a checkbox"
          ))
