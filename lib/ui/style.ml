type alignment = Start | End | Center | Fill
type t = { growth : float; vertical : alignment; horizontal : alignment }

let default = { growth = 1.; vertical = Center; horizontal = Fill }
let growth growth = { default with growth }
let vertical layout = { default with vertical = layout }
let horizontal layout = { default with horizontal = layout }

let ( & ) s1 s2 =
  let growth = if s2.growth = default.growth then s1.growth else s2.growth in
  let vertical =
    if s2.vertical = default.vertical then s1.vertical else s2.vertical
  in
  let horizontal =
    if s2.horizontal = default.horizontal then s1.horizontal else s2.horizontal
  in
  { growth; vertical; horizontal }
