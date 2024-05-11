open Gamelle_common.Geometry

module C1 = struct
  type t = { min : float; flex : float }

  let exact min = { min; flex = 0.0 }
  let zero = { min = 0.0; flex = 0.0 }
  let flex = { min = 0.0; flex = 1.0 }
  let add cst t = { t with min = t.min +. cst }
  let concat a b = { min = a.min +. b.min; flex = a.flex +. b.flex }
  let max a b = { min = max a.min b.min; flex = max a.flex b.flex }
end

type h = C1.t * (Box.t -> unit)
type t = C1.t * (float -> h)
(* width constraint, then height constraint, then render *)

let width ?(min = 0.0) ?(flex = 0.0) fn : t = ({ C1.min; flex }, fn)
let height ?(min = 0.0) ?(flex = 0.0) fn : h = ({ C1.min; flex }, fn)

let solve ?(width = fun w -> w) ?(height = fun h -> h) layout =
  let { C1.min = w; _ }, fn = layout in
  let w = max w (width w) in
  let { C1.min = h; _ }, fn = fn w in
  let h = max h (height h) in
  fn (Box.v Point.zero (Size.v w h))

let v ?(min_width = 0.0) ?(flex_width = 0.0) ?(min_height = 0.0)
    ?(flex_height = 0.0) fn =
  ( { C1.min = min_width; flex = flex_width },
    fun _ -> ({ C1.min = min_height; flex = flex_height }, fn) )

let fixed ?(width = 0.0) ?(height = 0.0) fn =
  (C1.exact width, fun _ -> (C1.exact height, fn))

let empty ?width ?height () = fixed ?width ?height (fun _ -> ())

let box_pad p p2 box =
  let p = Vec.v p p in
  let p2 = Vec.v p2 p2 in
  Box.v Point.(Box.top_left box + p) Size.(Box.size box - p2)

let pad p (width, fn) =
  let p2 = 2.0 *. p in
  ( C1.add p2 width,
    fun w ->
      let height, fn = fn (w -. p2) in
      (C1.add p2 height, fun box -> fn (box_pad p p2 box)) )

let horizontal lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.concat acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let inv_flex = if width.flex = 0.0 then 1.0 else 1.0 /. width.flex in
      let remaining_space = max 0.0 (actual_width -. width.C1.min) in
      let height, rev_lst =
        List.fold_left
          (fun (acc_height, rev_lst) (w, fn) ->
            let w = w.C1.min +. (remaining_space *. w.C1.flex *. inv_flex) in
            let h, fn = fn w in
            (C1.max acc_height h, (w, fn) :: rev_lst))
          (C1.zero, []) lst
      in
      let lst = List.rev rev_lst in
      ( height,
        fun box ->
          let h = Box.height box in
          let _ =
            List.fold_left
              (fun box (w, fn) ->
                fn (Box.v (Box.top_left box) (Size.v w h));

                let offset = Size.v w 0.0 in
                Box.v
                  Point.(Box.top_left box + offset)
                  Size.(Box.size box - offset))
              box lst
          in
          () ) )

let vertical lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.max acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let height, rev_lst =
        List.fold_left
          (fun (acc_height, rev_lst) (_, fn) ->
            let h, fn = fn actual_width in
            (C1.concat acc_height h, (h, fn) :: rev_lst))
          (C1.zero, []) lst
      in
      let lst = List.rev rev_lst in
      ( height,
        fun box ->
          let inv_flex =
            if height.flex = 0.0 then 1.0 else 1.0 /. height.flex
          in
          let remaining_space = max 0.0 (Box.height box -. height.C1.min) in
          let _ =
            List.fold_left
              (fun box (h, fn) ->
                let h =
                  h.C1.min +. (remaining_space *. h.C1.flex *. inv_flex)
                in
                fn (Box.v (Box.top_left box) (Size.v actual_width h));

                let offset = Size.v 0.0 h in
                Box.v
                  Point.(Box.top_left box + offset)
                  Size.(Box.size box - offset))
              box lst
          in
          () ) )

let rec intersperse e li =
  match li with
  | [] -> []
  | [ elt ] -> [ elt ]
  | elt :: li -> elt :: e :: intersperse e li

let horizontal ?gap lst =
  let lst =
    match gap with
    | Some gap when gap > 0.0 -> intersperse (empty ~width:gap ()) lst
    | _ -> lst
  in
  horizontal lst

let vertical ?gap lst =
  let lst =
    match gap with
    | Some gap when gap > 0.0 -> intersperse (empty ~height:gap ()) lst
    | _ -> lst
  in
  vertical lst

let over lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.max acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let height, rev_lst =
        List.fold_left
          (fun (acc_height, rev_lst) (_, fn) ->
            let h, fn = fn actual_width in
            (C1.max acc_height h, fn :: rev_lst))
          (C1.zero, []) lst
      in
      let lst = List.rev rev_lst in
      (height, fun box -> List.iter (fun fn -> fn box) lst) )

let padded p = function
  | [] -> empty ()
  | [ single ] -> pad p single
  | _ -> invalid_arg "Layout.padded"

let vclip height layout =
  let w_constraint, fn = layout in
  ( w_constraint,
    fun actual_width ->
      let h_constraint, fn = fn actual_width in
      ( C1.exact height,
        fun box ->
          let box =
            Box.v (Box.top_left box)
              (Size.v (Box.width box)
                 (max h_constraint.C1.min (Box.height box)))
          in
          fn box ) )

let center layout =
  let w, fn = layout in
  ( C1.max w C1.flex,
    fun actual_width ->
      let h, fn = fn actual_width in
      ( C1.max h C1.flex,
        fun box ->
          let width = if w.C1.flex = 0.0 then w.C1.min else Box.width box in
          let height = if h.C1.flex = 0.0 then h.C1.min else Box.height box in
          let box = Box.v_center (Box.center box) (Size.v width height) in
          fn box ) )

let center layout = center (vertical layout)
