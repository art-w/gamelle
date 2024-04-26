type t = Point.t list

let v li = li
let to_list p = p

let center = function
  | [] -> assert false
  | hd :: _ as ps ->
      let rec go ~area ~cx ~cy = function
        | [] | [ _ ] ->
            let area6 = 3.0 *. area in
            Vec.v (cx /. area6) (cy /. area6)
        | p0 :: p1 :: ps ->
            let x0, y0 = Vec.to_tuple p0 in
            let x1, y1 = Vec.to_tuple p1 in
            let delta = (x0 *. y1) -. (x1 *. y0) in
            go ~area:(area +. delta)
              ~cx:(cx +. ((x0 +. x1) *. delta))
              ~cy:(cy +. ((y0 +. y1) *. delta))
              (p1 :: ps)
      in
      go ~area:0.0 ~cx:0.0 ~cy:0.0 (hd :: List.rev ps)

let signed_area = function
  | [] -> 0.0
  | hd :: _ as ps ->
      let rec go ~area = function
        | [] | [ _ ] -> area /. 2.0
        | p0 :: p1 :: ps ->
            let x0, y0 = Vec.to_tuple p0 in
            let x1, y1 = Vec.to_tuple p1 in
            let delta = (x0 *. y1) -. (x1 *. y0) in
            go ~area:(area +. delta) (p1 :: ps)
      in
      go ~area:0.0 (hd :: List.rev ps)

let segments pts =
  match pts with
  | [] -> []
  | first :: _ ->
      let rec go acc = function
        | [] -> acc
        | [ last ] -> Segment.v last first :: acc
        | a :: b :: xs -> go (Segment.v a b :: acc) (b :: xs)
      in
      go [] pts

let translate poly v = List.map (Vec.( + ) v) poly
let map_points f poly = List.map f poly

let bounding_box poly =
  let x_min, x_max, y_min, y_max =
    List.fold_left
      (fun (x_min, x_max, y_min, y_max) p ->
        ( Float.min (Point.x p) x_min,
          Float.max (Point.x p) x_max,
          Float.min (Point.y p) y_min,
          Float.max (Point.y p) y_max ))
      (Float.infinity, Float.neg_infinity, Float.infinity, Float.neg_infinity)
      poly
  in
  Box.v_corners (Point.v x_min y_min) (Point.v x_max y_max)
