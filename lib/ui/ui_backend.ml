open Gamelle_backend
open Draw_geometry

type id = { loc_stack : string list; _hint : int option }
type renderer = Layout.t

type t = {
  io : io ref;
  mutable renderers : renderer list;
  mutable loc_stack : string list;
}

let update_loc (ui, _loc) loc = (ui, loc)
let id (ui, loc) = { loc_stack = loc :: ui.loc_stack; _hint = None }

module State (Value : sig
  type t
end) =
struct
  module H = Hashtbl.Make (struct
    type t = id

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let state : Value.t ref H.t = H.create 16

  let find ui default =
    let key = id ui in
    try H.find state key
    with Not_found ->
      let r = ref default in
      H.replace state key r;
      r
end

type ui = t * string

let get_io (ui, _loc) = !(ui.io)
let push_renderer ~ui renderer = ui.renderers <- renderer :: ui.renderers
let draw_layout (ui, _loc) layout = push_renderer ~ui layout

let draw ui ?min_width ?min_height ?flex_width ?flex_height fn =
  draw_layout ui
    (Layout.v ?min_width ?min_height ?flex_width ?flex_height
       (fn ~io:(get_io ui)))

let draw_size ui s fn =
  draw ~min_width:(Size.width s) ~min_height:(Size.height s) ui fn

let padding = 4.
let padding_x = Vec.v padding 0.
let padding_y = Vec.v 0. padding
let padding_xy = Vec.(padding_x + padding_y)
let fg = Gruvbox.Light.fg
let bg = Gruvbox.Light.bg
let bg' = Gruvbox.Light.bg1
let highlight = Gruvbox.Light.blue
let lowlight = Gruvbox.Light.gray

let nest_loc (ui, loc) fn =
  let loc_stack = ui.loc_stack in
  ui.loc_stack <- loc :: loc_stack;
  let result = fn () in
  ui.loc_stack <- loc_stack;
  result

let parent (ui, loc) layout fn =
  nest_loc (ui, loc) @@ fun () ->
  let siblings = ui.renderers in
  ui.renderers <- [];
  let result = fn () in
  let children = ui.renderers in
  let self = layout (List.rev children) in
  ui.renderers <- self :: siblings;
  result

let vclip (ui, _loc) ?(offset = Vec.zero) box f =
  let io = !(ui.io) in
  ui.io :=
    View.clip_events true
    @@ View.clip (Box.translate offset box)
    @@ View.translate Vec.(-1.0 * offset) io;
  let result =
    parent (ui, _loc)
      (function [ single ] -> Layout.vclip 0.0 single | _ -> assert false)
      f
  in
  ui.io := io;
  result
