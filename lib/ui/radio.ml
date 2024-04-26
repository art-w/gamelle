open Draw_geometry
open Ui_backend
open Widget_builder

type 'a params = ('a * string) list
type state = int option
type 'a return = 'a option
type Ui_backend.state += Radio of state

let default _ = None
let construct_state b = Radio b
let destruct_state s = match s with Radio b -> b | _ -> raise IdTypeMismatch
let size_one = Checkbox.size
let sizes ~ts options = List.map (fun (_, label) -> size_one ~ts label) options
let size ~ts options = aggregate_sizes ~dir:V (sizes ~ts options)
let render_one = Checkbox.render

let layout ~io options box =
  let sizes = sizes ~ts:(Text.size ~io ~size:font_size) options in
  let weights = List.map (fun _ -> 1.) options in
  layout_boxes ~dir:V box weights sizes

let render ~io options n_checked box =
  let boxes = layout ~io options box in
  List.iteri
    (fun i (box, (_, label)) ->
      let is_checked = match n_checked with None -> false | Some n -> n = i in
      render_one ~io label is_checked box)
    (List.combine boxes options)

(* We don't use List.find_index for backwards compatibility *)
let find_index p =
  let rec aux i = function
    | [] -> None
    | a :: l -> if p a then Some i else aux (i + 1) l
  in
  aux 0

let update ~io options previous box =
  let boxes = layout ~io options box in
  find_index (is_clicked ~io) boxes |> function
  | None -> previous
  | v -> if v = previous then None else v

let result options n_checked =
  Option.map (List.nth options) n_checked |> Option.map fst

let size_radio = size
let render_radio = render

let destruct_result params result =
  Option.bind result (fun result ->
      find_index (fun (id, _label) -> id = result) params)

let v cap ?id ?init ?size ?style ?render params =
  elt ~construct_state ~destruct_state ~default ~size:size_radio
    ~render:render_radio ~update ~destruct_result ~result () cap ?id ?init
    ?style ?size ?render params
