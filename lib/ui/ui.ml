open Draw_geometry
open Ui_backend
include Widgets

let text_input = Text_input.v
let vscroll = Vscroll.v
let slider = Slider.v

let window ~io ?width ?height fn =
  let t = { io = ref io; renderers = []; loc_stack = [] } in
  let ui = (t, "root") in
  let result =
    over ui @@ fun () ->
    draw ui (Box.fill ~color:Gruvbox.Light.bg_s);
    padding ui 10.0 @@ fun () -> vertical ui (fun () -> fn ui)
  in
  match t.renderers with
  | [ single ] ->
      Layout.solve ?width ?height single;
      Ui_backend.clean_old_states ();
      result
  | _ -> assert false

let update_loc = Ui_backend.update_loc
let nest_loc = Ui_backend.nest_loc
