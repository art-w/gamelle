open Gamelle_backend
open Draw_geometry

type t = Segment of segment | Circle of circle | Polygon of point list

let segment seg = Segment seg
let circle c = Circle c
let polygon pts = Polygon pts

let rect box =
  polygon [ Box.tl_pt box; Box.tr_pt box; Box.br_pt box; Box.bl_pt box ]

let draw ~io ~color = function
  | Segment s -> draw_line ~io ~color s
  | Circle c -> draw_circle ~io ~color c
  | Polygon pts -> draw_poly ~io ~color pts

let fill ~io ~color = function
  | Segment s -> draw_line ~io ~color s
  | Circle c -> fill_circle ~io ~color c
  | Polygon pts -> fill_poly ~io ~color pts

let translate dxy = function
  | Segment s -> Segment (Segment.translate s dxy)
  | Circle c -> Circle (Circle.translate c dxy)
  | Polygon pts -> Polygon (List.map (Vec.( + ) dxy) pts)

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( ~- ) = ( ~-. )
let ( * ) = ( *. )
let ( / ) = ( /. )

let rotate_around ~angle ~center shape =
  let rot = Vec.rotate_around ~angle ~center in
  match shape with
  | Circle c -> Circle (Circle.map_center rot c)
  | Segment s -> Segment (Segment.map_points rot s)
  | Polygon pts -> Polygon (List.map rot pts)

let polygon_center = function
  | [] -> assert false
  | hd :: _ as ps ->
      let rec go ~area ~cx ~cy = function
        | [] | [ _ ] ->
            let area6 = 3.0 * area in
            Vec.v (cx / area6) (cy / area6)
        | p0 :: p1 :: ps ->
            let x0, y0 = Vec.to_tuple p0 in
            let x1, y1 = Vec.to_tuple p1 in
            let delta = (x0 * y1) - (x1 * y0) in
            go ~area:(area + delta)
              ~cx:(cx + ((x0 + x1) * delta))
              ~cy:(cy + ((y0 + y1) * delta))
              (p1 :: ps)
      in
      go ~area:0.0 ~cx:0.0 ~cy:0.0 (hd :: List.rev ps)

let center = function
  | Circle c -> Circle.center c
  | Segment s -> Segment.center s
  | Polygon pts -> polygon_center pts

let polygon_signed_area = function
  | [] -> 0.0
  | hd :: _ as ps ->
      let rec go ~area = function
        | [] | [ _ ] -> area /. 2.0
        | p0 :: p1 :: ps ->
            let x0, y0 = Vec.to_tuple p0 in
            let x1, y1 = Vec.to_tuple p1 in
            let delta = (x0 * y1) - (x1 * y0) in
            go ~area:(area + delta) (p1 :: ps)
      in
      go ~area:0.0 (hd :: List.rev ps)

let signed_area = function
  | Circle c -> Circle.area c
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
        | [ last ] -> Segment.v last first :: acc
        | a :: b :: xs -> go (Segment.v a b :: acc) (b :: xs)
      in
      go [] pts

let nearest_point_segment pt s =
  let p0, p1 = Segment.to_tuple s in
  let l2 = Vec.(norm2 (p0 - p1)) in
  if l2 = 0.0 then p0
  else
    let t = max 0. (min 1. (Vec.(dot (pt - p0) (p1 - p0)) / l2)) in
    Vec.(p0 + (t * (p1 - p0)))

let float_equal a b = abs_float (a -. b) < 0.01

let nearest_points pt = function
  | Circle c ->
      let center = Circle.center c and radius = Circle.radius c in
      let d = Vec.(unit (pt - center)) in
      [ (Vec.(center + (radius * d)), d) ]
  | Segment s ->
      let p0, p1 = Segment.to_tuple s in
      let pt = nearest_point_segment pt s in
      [ (pt, Vec.(unit @@ ortho (p0 - p1))) ]
  | Polygon pts -> (
      match segments_of_polygon pts with
      | [] -> invalid_arg "empty polygon"
      | first :: ss ->
          let compute segment =
            let p0, p1 = Segment.to_tuple segment in
            let found = nearest_point_segment pt segment in
            let dist2 = Vec.(norm2 (pt - found)) in
            (dist2, [ (found, Vec.(unit @@ ortho (p0 - p1))) ])
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
  | (npt, _) :: _ -> Vec.(norm2 (pt - npt))

let intersection_segment_circle s c =
  let p0, p1 = Segment.to_tuple s in
  let center = Circle.center c and radius = Circle.radius c in
  let d = Vec.(p1 - p0) in
  let f = Vec.(p0 - center) in
  let a = Vec.dot d d in
  let b = 2.0 * Vec.dot f d in
  let c = Vec.dot f f - (radius * radius) in
  let discriminant = (b * b) - (4. * a * c) in
  if discriminant < 0. then []
  else
    let discriminant = sqrt discriminant in
    let t1 = (-b - discriminant) / (2. * a) in
    let t2 = (-b + discriminant) / (2. * a) in
    let pt1 = if t1 >= 0. && t1 <= 1.0 then [ Vec.(p0 + (t1 * d)) ] else [] in
    let pt2 = if t2 >= 0. && t2 <= 1. then [ Vec.(p0 + (t2 * d)) ] else [] in
    pt1 @ pt2

let segment_intersection p q =
  let p0, p1 = Segment.to_tuple p and q0, q1 = Segment.to_tuple q in
  let p0_x, p0_y = Vec.to_tuple p0 in
  let p1_x, p1_y = Vec.to_tuple p1 in
  let p2_x, p2_y = Vec.to_tuple q0 in
  let p3_x, p3_y = Vec.to_tuple q1 in
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
    Some (Point.v i_x i_y)
  else None

let rec intersections a b =
  match (a, b) with
  | Segment s, Circle c | Circle c, Segment s -> intersection_segment_circle s c
  | Segment p, Segment q -> (
      match segment_intersection p q with None -> [] | Some pt -> [ pt ])
  | Circle c, Circle c' -> Circle.intersection c c'
  | Polygon pts, other | other, Polygon pts ->
      List.concat_map
        (fun s -> intersections (Segment s) other)
        (segments_of_polygon pts)

let intersects a b =
  match (a, b) with
  | Circle c, Circle c' -> Circle.intersects c c'
  | _ -> intersections a b <> []

let mem pt = function
  | Circle c -> Circle.mem c pt
  | Segment _ -> false
  | Polygon pts ->
      let x, y = Vec.to_tuple pt in
      List.fold_left
        (fun inside s ->
          let p0, p1 = Segment.to_tuple s in
          let vx0, vy0 = Vec.to_tuple p0 in
          let vx1, vy1 = Vec.to_tuple p1 in
          if
            vy0 > y <> (vy1 > y)
            && x < ((vx0 - vx1) * (y - vy1) / (vy0 - vy1)) + vx1
          then not inside
          else inside)
        false (segments_of_polygon pts)

let separation_axis_circle c shape =
  let center = Circle.center c and radius = Circle.radius c in
  let pt, _ = List.hd @@ nearest_points center shape in
  let axis = Vec.(center - pt) in
  let dist2 = Vec.(norm2 axis) in
  if mem center shape then
    let delta = radius +. sqrt dist2 in
    Some Vec.(delta * unit axis)
  else if dist2 > radius * radius then None
  else
    let delta = radius -. sqrt dist2 in
    Some Vec.(delta * unit axis)

let project axis = function
  | [] -> invalid_arg "project: empty list"
  | pt :: pts ->
      let v = Vec.dot pt axis in
      List.fold_left
        (fun (vmin, vmax) pt ->
          let v = Vec.dot pt axis in
          (min v vmin, max v vmax))
        (v, v) pts

let separation_axis_polygon a b =
  let rec go opt_best = function
    | [] -> opt_best
    | seg :: rest -> (
        let p0, p1 = Segment.to_tuple seg in
        let axis = Vec.(unit @@ ortho (p0 - p1)) in
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
  match go None ss with None -> None | Some (d, n) -> Some Vec.(d * n)

let separation_axis a b =
  match (a, b) with
  | Circle c, shape -> separation_axis_circle c shape
  | shape, Circle c -> (
      match separation_axis_circle c shape with
      | None -> None
      | Some n -> Some Vec.(-1.0 * n))
  | Polygon a, Polygon b -> separation_axis_polygon a b
  | Segment s, Polygon b ->
      let a0, a1 = Segment.to_tuple s in
      separation_axis_polygon [ a0; a1 ] b
  | Polygon a, Segment s ->
      let b0, b1 = Segment.to_tuple s in
      separation_axis_polygon a [ b0; b1 ]
  | Segment s, Segment s' ->
      let a0, a1 = Segment.to_tuple s in
      let b0, b1 = Segment.to_tuple s' in
      separation_axis_polygon [ a0; a1 ] [ b0; b1 ]

let separation_axis a b =
  match separation_axis a b with
  | Some n when Vec.norm2 n = 0.0 -> None
  | found -> found

module Set_v2 = Set.Make (struct
  type t = Vec.t

  let eps = 1.0 /. 0.01
  let int = int_of_float

  let compare a b =
    let ax, ay = Vec.(to_tuple (eps * a)) in
    let bx, by = Vec.(to_tuple (eps * b)) in
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
        let dist2 = Vec.(norm2 (npt - pt)) in
        (dist2, Set_v2.singleton pt)
      in
      List.fold_left
        (fun acc pt -> best_distance acc (compute pt))
        (compute hd) pts

let rec contact_points a b =
  match (a, b) with
  | Circle c, Circle c' ->
      let a = Circle.center c and b = Circle.center c' in
      let dist = Vec.(norm (a - b)) in
      (dist, Set_v2.singleton Vec.(a + (0.5 * (a - b))))
  | Polygon a, Circle c | Circle c, Polygon a ->
      let b = Circle.center c in
      let a, _ = List.hd @@ nearest_points b (Polygon a) in
      let dist = Vec.(norm (a - b)) in
      (dist, Set_v2.singleton a)
  | Polygon a_pts, Polygon b_pts ->
      let fst = contact_points_rectangle a_pts b in
      let snd = contact_points_rectangle b_pts a in
      best_distance fst snd
  | Segment seg, b ->
      let a0, a1 = Segment.to_tuple seg in
      contact_points (Polygon [ a0; a1 ]) b
  | a, Segment seg ->
      let b0, b1 = Segment.to_tuple seg in
      contact_points a (Polygon [ b0; b1 ])

let contact_points a b =
  let dist, lst = contact_points a b in
  (dist, Set_v2.elements lst)
