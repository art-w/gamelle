type t = float -> float

let linear t = t
let in_quad t = t *. t
let out_quad t = 1.0 -. in_quad (1.0 -. t)

let in_out_quad t =
  if t < 0.5 then 2. *. t *. t
  else
    let t = 2.0 -. (2.0 *. t) in
    1.0 -. (t *. t /. 2.0)

let in_cubic t = t *. t *. t
let out_cubic t = 1.0 -. in_cubic (1.0 -. t)

let in_out_cubic t =
  if t < 0.5 then in_cubic (2.0 *. t) /. 2.0
  else (1.0 +. out_cubic (2.0 *. (t -. 0.5))) /. 2.0

let in_quart t = t *. t *. t *. t
let out_quart t = 1.0 -. in_quart (1.0 -. t)

let in_out_quart t =
  if t < 0.5 then in_quart (2.0 *. t) /. 2.0
  else (1.0 +. out_quart (2.0 *. (t -. 0.5))) /. 2.0

let in_back t =
  let a = 1.70158 in
  let b = a +. 1.0 in
  let t2 = t *. t in
  (b *. t *. t2) -. (a *. t2)

let out_back t = 1.0 -. in_back (1.0 -. t)

let in_out_back t =
  if t < 0.5 then in_back (2.0 *. t) /. 2.0
  else (1.0 +. out_back (2.0 *. (t -. 0.5))) /. 2.0

let out_bounce t =
  let n1 = 7.5625 in
  let d1 = 2.75 in
  let inv_d1 = 1.0 /. d1 in
  if t < inv_d1 then n1 *. t *. t
  else if t < 2.0 *. inv_d1 then
    let t = t -. (1.5 *. inv_d1) in
    (n1 *. t *. t) +. 0.75
  else if t < 2.5 *. inv_d1 then
    let t = t -. (2.25 *. inv_d1) in
    (n1 *. t *. t) +. 0.9375
  else
    let t = t -. (2.625 *. inv_d1) in
    (n1 *. t *. t) +. 0.984375

let in_bounce t = 1.0 -. out_bounce (1.0 -. t)

let in_out_bounce t =
  if t < 0.5 then in_bounce (2.0 *. t) /. 2.0
  else (1.0 +. out_bounce (2.0 *. (t -. 0.5))) /. 2.0

let in_elastic t =
  let c4 = 8.0 *. atan 1.0 /. 3. in
  let t = 10.0 *. (t -. 1.0) in
  -.((2.0 ** t) *. sin ((t -. 0.75) *. c4))

let out_elastic t = 1.0 -. in_elastic (1.0 -. t)

let in_out_elastic t =
  if t < 0.5 then in_elastic (2.0 *. t) /. 2.0
  else (1.0 +. out_elastic (2.0 *. (t -. 0.5))) /. 2.0
