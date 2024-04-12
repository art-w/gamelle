open Gamelle_backend
open Gamelle_common
open Geometry

type t = Segment of p2 * p2 | Circle of p2 * size1 | Polygon of p2 list

let segment p0 p1 = Segment (p0, p1)
let circle center radius = Circle (center, radius)
let polygon pts = Polygon pts

let rect box =
  polygon [ Box2.tl_pt box; Box2.tr_pt box; Box2.br_pt box; Box2.bl_pt box ]

let draw ~io ~color = function
  | Segment (p0, p1) -> draw_line ~io ~color (Segment.v p0 p1)
  | Circle (center, radius) -> draw_circle ~io ~color (Circle.v center radius)
  | Polygon pts -> draw_poly ~io ~color pts

let fill ~io ~color = function
  | Segment (p0, p1) -> draw_line ~io ~color (Segment.v p0 p1)
  | Circle (center, radius) -> fill_circle ~io ~color (Circle.v center radius)
  | Polygon pts -> fill_poly ~io ~color pts

let translate dxy = function
  | Segment (p0, p1) -> Segment (V2.(p0 + dxy), V2.(p1 + dxy))
  | Circle (center, radius) -> Circle (V2.(center + dxy), radius)
  | Polygon pts -> Polygon (List.map (V2.( + ) dxy) pts)

let square x = x *. x
let ( + ) = ( +. )
let ( - ) = ( -. )
let ( ~- ) = ( ~-. )
let ( * ) = ( *. )
let ( / ) = ( /. )

let rotate_v2_around ~angle:(cos, sin) ~center pt =
  let x, y = V2.to_tuple @@ V2.(pt - center) in
  let pt = V2.v ((x * cos) - (y * sin)) ((y * cos) + (x * sin)) in
  V2.(pt + center)

let rotate_around ~angle ~center shape =
  let rot = rotate_v2_around ~angle ~center in
  match shape with
  | Circle (center, radius) -> Circle (rot center, radius)
  | Segment (a, b) -> Segment (rot a, rot b)
  | Polygon pts -> Polygon (List.map rot pts)

let polygon_center = function
  | [] -> assert false
  | hd :: _ as ps ->
      let rec go ~area ~cx ~cy = function
        | [] | [ _ ] ->
            let area6 = 3.0 * area in
            V2.v (cx / area6) (cy / area6)
        | p0 :: p1 :: ps ->
            let x0, y0 = V2.to_tuple p0 in
            let x1, y1 = V2.to_tuple p1 in
            let delta = (x0 * y1) - (x1 * y0) in
            go ~area:(area + delta)
              ~cx:(cx + ((x0 + x1) * delta))
              ~cy:(cy + ((y0 + y1) * delta))
              (p1 :: ps)
      in
      go ~area:0.0 ~cx:0.0 ~cy:0.0 (hd :: List.rev ps)

let center = function
  | Circle (center, _) -> center
  | Segment (a, b) -> V2.(0.5 * (a + b))
  | Polygon pts -> polygon_center pts

let polygon_signed_area = function
  | [] -> 0.0
  | hd :: _ as ps ->
      let rec go ~area = function
        | [] | [ _ ] -> area /. 2.0
        | p0 :: p1 :: ps ->
            let x0, y0 = V2.to_tuple p0 in
            let x1, y1 = V2.to_tuple p1 in
            let delta = (x0 * y1) - (x1 * y0) in
            go ~area:(area + delta) (p1 :: ps)
      in
      go ~area:0.0 (hd :: List.rev ps)

let pi = 4.0 * atan 1.0

let signed_area = function
  | Circle (_, radius) -> pi * radius * radius
  | Segment _ -> 1.0
  | Polygon pts -> polygon_signed_area pts

let rotate ?center:opt_center ~angle shape =
  let center =
    match opt_center with Some center -> center | None -> center shape
  in
  rotate_around ~angle:(cos angle, sin angle) ~center shape

let segments_of_polygon pts =
  match pts with
  | [] -> []
  | first :: _ ->
      let rec go acc = function
        | [] -> acc
        | [ last ] -> (last, first) :: acc
        | a :: b :: xs -> go ((a, b) :: acc) (b :: xs)
      in
      go [] pts

let nearest_point_segment pt (p0, p1) =
  let l2 = V2.(norm2 (p0 - p1)) in
  if l2 = 0.0 then p0
  else
    let t = max 0. (min 1. (V2.(dot (pt - p0) (p1 - p0)) / l2)) in
    V2.(p0 + (t * (p1 - p0)))

let float_equal a b = abs_float (a -. b) < 0.01

let nearest_points pt = function
  | Circle (center, radius) ->
      let d = V2.(unit (pt - center)) in
      [ (V2.(center + (radius * d)), d) ]
  | Segment (p0, p1) ->
      let pt = nearest_point_segment pt (p0, p1) in
      [ (pt, V2.(unit @@ ortho (p0 - p1))) ]
  | Polygon pts -> (
      match segments_of_polygon pts with
      | [] -> invalid_arg "empty polygon"
      | first :: ss ->
          let compute ((p0, p1) as segment) =
            let found = nearest_point_segment pt segment in
            let dist2 = V2.(norm2 (pt - found)) in
            (dist2, [ (found, V2.(unit @@ ortho (p0 - p1))) ])
          in
          let _, found =
            List.fold_left
              (fun (best_distance2, bests) segment ->
                let dist2, found = compute segment in
                if float_equal dist2 best_distance2 then (dist2, found @ bests)
                else if dist2 < best_distance2 then (dist2, found)
                else (best_distance2, bests))
              (compute first) ss
          in
          found)

let distance2 pt shape =
  match nearest_points pt shape with
  | [] -> invalid_arg "distance2"
  | (npt, _) :: _ -> V2.(norm2 (pt - npt))

let intersection_segment_circle p0 p1 center radius =
  let d = V2.(p1 - p0) in
  let f = V2.(p0 - center) in
  let a = V2.dot d d in
  let b = 2.0 * V2.dot f d in
  let c = V2.dot f f - (radius * radius) in
  let discriminant = (b * b) - (4. * a * c) in
  if discriminant < 0. then []
  else
    let discriminant = sqrt discriminant in
    let t1 = (-b - discriminant) / (2. * a) in
    let t2 = (-b + discriminant) / (2. * a) in
    let pt1 = if t1 >= 0. && t1 <= 1.0 then [ V2.(p0 + (t1 * d)) ] else [] in
    let pt2 = if t2 >= 0. && t2 <= 1. then [ V2.(p0 + (t2 * d)) ] else [] in
    pt1 @ pt2

let segment_intersection (p0, p1) (q0, q1) =
  let p0_x, p0_y = V2.to_tuple p0 in
  let p1_x, p1_y = V2.to_tuple p1 in
  let p2_x, p2_y = V2.to_tuple q0 in
  let p3_x, p3_y = V2.to_tuple q1 in
  let s1_x = p1_x - p0_x in
  let s1_y = p1_y - p0_y in
  let s2_x = p3_x - p2_x in
  let s2_y = p3_y - p2_y in
  let s =
    ((-s1_y * (p0_x - p2_x)) + (s1_x * (p0_y - p2_y)))
    / ((-s2_x * s1_y) + (s1_x * s2_y))
  in
  let t =
    ((s2_x * (p0_y - p2_y)) - (s2_y * (p0_x - p2_x)))
    / ((-s2_x * s1_y) + (s1_x * s2_y))
  in
  if s >= 0.0 && s <= 1.0 && t >= 0.0 && t <= 1.0 then
    let i_x = p0_x + (t * s1_x) in
    let i_y = p0_y + (t * s1_y) in
    Some (P2.v i_x i_y)
  else None

let rec intersections a b =
  match (a, b) with
  | Segment (p0, p1), Circle (center, radius)
  | Circle (center, radius), Segment (p0, p1) ->
      intersection_segment_circle p0 p1 center radius
  | Segment (p0, p1), Segment (q0, q1) -> (
      match segment_intersection (p0, p1) (q0, q1) with
      | None -> []
      | Some pt -> [ pt ])
  | Circle (c0, r0), Circle (c1, r1) ->
      let dist2 = V2.(norm2 (c1 - c0)) in
      if dist2 >= square (r0 +. r1) || dist2 <= square (r0 -. r1) then []
      else
        let d = sqrt dist2 in
        let l = (square r0 -. square r1 +. dist2) /. (2.0 *. d) in
        let h = sqrt (square r0 -. square l) in
        let x1, y1 = V2.to_tuple c0 in
        let x2, y2 = V2.to_tuple c1 in
        let ld = l /. d in
        let hd = h /. d in
        let base_x = (ld *. (x2 -. x1)) +. x1 in
        let base_y = (ld *. (y2 -. y1)) +. y1 in
        if hd = 0.0 then [ P2.v base_x base_y ]
        else
          let px = hd *. (y2 -. y1) in
          let py = hd *. (x2 -. x1) in
          [
            P2.v (base_x +. px) (base_y -. py);
            P2.v (base_x -. px) (base_y +. py);
          ]
  | Polygon pts, other | other, Polygon pts ->
      List.concat_map
        (fun (p0, p1) -> intersections (Segment (p0, p1)) other)
        (segments_of_polygon pts)

let intersects a b =
  match (a, b) with
  | Circle (c0, r0), Circle (c1, r1) ->
      let dist2 = V2.(norm2 (c1 - c0)) in
      let r2 = square (r0 +. r1) in
      dist2 < r2
  | _ -> intersections a b <> []

let mem pt = function
  | Circle (center, radius) ->
      let dist2 = V2.(norm2 (center - pt)) in
      dist2 <= radius * radius
  | Segment _ -> false
  | Polygon pts ->
      let x, y = V2.to_tuple pt in
      List.fold_left
        (fun inside (p0, p1) ->
          let vx0, vy0 = V2.to_tuple p0 in
          let vx1, vy1 = V2.to_tuple p1 in
          if
            vy0 > y <> (vy1 > y)
            && x < ((vx0 - vx1) * (y - vy1) / (vy0 - vy1)) + vx1
          then not inside
          else inside)
        false (segments_of_polygon pts)

let separation_axis_circle center radius shape =
  let pt, _ = List.hd @@ nearest_points center shape in
  let axis = V2.(center - pt) in
  let dist2 = V2.(norm2 axis) in
  if mem center shape then
    let delta = radius +. sqrt dist2 in
    Some V2.(delta * unit axis)
  else if dist2 > radius * radius then None
  else
    let delta = radius -. sqrt dist2 in
    Some V2.(delta * unit axis)

let project axis = function
  | [] -> invalid_arg "project: empty list"
  | pt :: pts ->
      let v = V2.dot pt axis in
      List.fold_left
        (fun (vmin, vmax) pt ->
          let v = V2.dot pt axis in
          (min v vmin, max v vmax))
        (v, v) pts

let separation_axis_polygon a b =
  let rec go opt_best = function
    | [] -> opt_best
    | (p0, p1) :: rest -> (
        let axis = V2.(unit @@ ortho (p0 - p1)) in
        let amin, amax = project axis a in
        let bmin, bmax = project axis b in
        let d1 = bmin -. amax in
        let d2 = bmax -. amin in
        if d1 >= 0.0 then None
        else if d2 <= 0.0 then None
        else
          let overlap = if abs_float d1 < abs_float d2 then d1 else d2 in
          match opt_best with
          | None -> go (Some (overlap, axis)) rest
          | Some (best, _) when abs_float overlap < abs_float best ->
              go (Some (overlap, axis)) rest
          | _ -> go opt_best rest)
  in
  let ss = List.rev_append (segments_of_polygon a) (segments_of_polygon b) in
  match go None ss with None -> None | Some (d, n) -> Some V2.(d * n)

let separation_axis a b =
  match (a, b) with
  | Circle (center, radius), shape -> separation_axis_circle center radius shape
  | shape, Circle (center, radius) -> (
      match separation_axis_circle center radius shape with
      | None -> None
      | Some n -> Some V2.(-1.0 * n))
  | Polygon a, Polygon b -> separation_axis_polygon a b
  | Segment (a0, a1), Polygon b -> separation_axis_polygon [ a0; a1 ] b
  | Polygon a, Segment (b0, b1) -> separation_axis_polygon a [ b0; b1 ]
  | Segment (a0, a1), Segment (b0, b1) ->
      separation_axis_polygon [ a0; a1 ] [ b0; b1 ]

let separation_axis a b =
  match separation_axis a b with
  | Some n when V2.norm2 n = 0.0 -> None
  | found -> found

module Set_v2 = Set.Make (struct
  type t = V2.t

  let eps = 1.0 /. 0.01
  let int = int_of_float

  let compare a b =
    let ax, ay = V2.(to_tuple (eps * a)) in
    let bx, by = V2.(to_tuple (eps * b)) in
    Stdlib.compare (int ax, int ay) (int bx, int by)
end)

let best_distance (x_dist, xs) (y_dist, ys) =
  if float_equal x_dist y_dist then (x_dist, Set_v2.union xs ys)
  else if x_dist < y_dist then (x_dist, xs)
  else (y_dist, ys)

let contact_points_rectangle pts rect =
  match pts with
  | [] -> invalid_arg "contact_points: empty list"
  | hd :: pts ->
      let compute pt =
        let found = nearest_points pt rect in
        let npt, _ = List.hd found in
        let dist2 = V2.(norm2 (npt - pt)) in
        (dist2, Set_v2.singleton pt)
      in
      List.fold_left
        (fun acc pt -> best_distance acc (compute pt))
        (compute hd) pts

let rec contact_points a b =
  match (a, b) with
  | Circle (a, _ra), Circle (b, _rb) ->
      let dist = V2.(norm (a - b)) in
      (dist, Set_v2.singleton V2.(a + (0.5 * (a - b))))
  | Polygon a, Circle (b, _rb) | Circle (b, _rb), Polygon a ->
      let a, _ = List.hd @@ nearest_points b (Polygon a) in
      let dist = V2.(norm (a - b)) in
      (dist, Set_v2.singleton a)
  | Polygon a_pts, Polygon b_pts ->
      let fst = contact_points_rectangle a_pts b in
      let snd = contact_points_rectangle b_pts a in
      best_distance fst snd
  | Segment (a0, a1), b -> contact_points (Polygon [ a0; a1 ]) b
  | a, Segment (b0, b1) -> contact_points a (Polygon [ b0; b1 ])

let contact_points a b =
  let dist, lst = contact_points a b in
  (dist, Set_v2.elements lst)
