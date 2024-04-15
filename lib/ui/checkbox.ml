open Draw_geometry
open Ui_backend
open Widget_builder

type params = string
type state = bool
type return = bool
type Ui_backend.state += Checkbox of state

let construct_state b = Checkbox b

let destruct_state s =
  match s with Checkbox b -> b | _ -> raise IdTypeMismatch

let size ~ts text =
  let text_size = ts text in
  let check'box'_size = Size.h text_size in
  Vec.(text_size + (2. * padding_xy) + Size.v (check'box'_size +. padding) 0.)

let render ~io text is_checked box =
  let pos = Box.o box in
  let check'box'_size = Box.h box -. (2. *. padding) in
  let pos = Vec.(pos + padding_y) in
  let check'box' =
    Box.(v Vec.(o box + padding_xy) Size.(v check'box'_size check'box'_size))
  in
  Box.fill ~io ~color:bg' box;
  Box.draw ~io ~color:fg box;
  Box.fill ~io ~color:bg check'box';
  Box.draw ~io ~color:fg check'box';
  (if is_checked then
     let ticked'box' =
       Box.(
         v
           Vec.(o check'box' + padding_xy)
           Vec.(Box.size check'box' - (2. * padding_xy)))
     in
     Box.fill ~io ~color:highlight ticked'box');
  Text.draw ~io ~color:fg ~size:font_size text
    Vec.(pos + padding_x + v check'box'_size 0. + padding_x)

let update ~io _text previous_is_checked box =
  let is_clicked = is_clicked ~io box in
  if is_clicked then not previous_is_checked else previous_is_checked

let result _ is_clicked = is_clicked

let v =
  elt ~construct_state ~destruct_state
    ~default:(fun _ -> false)
    ~size ~render ~update ~result ()
