open Draw_geometry
open Ui_backend
open Widget_builder

type params = string
type state = bool
type return = bool
type Ui_backend.state += Button of state

let construct_state b = Button b
let destruct_state s = match s with Button b -> b | _ -> raise IdTypeMismatch

let size ~ts text =
  let text_size = ts text in
  Vec.(text_size + (2. * padding_xy))

let render ~io text _is_clicked box =
  Box.fill ~io ~color:bg' box;
  Box.draw ~io ~color:fg box;
  let text_size = Text.size ~io text in
  let text_box = Box.(v_mid (mid box) text_size) in
  Text.draw ~io ~color:fg ~at:(Box.o text_box) text

let update ~io _text _old_state box = is_clicked ~io box
let result _ b = b
let destruct_result _ = Fun.id

let v =
  elt ~construct_state ~destruct_state
    ~default:(fun _ -> false)
    ~size ~render ~update ~destruct_result ~result ()
