open Gamelle

type state = { sound : Sound.t option; playing : bool }

let bar_w = 500.0
let bar_h = 24.0
let bar_x = 50.0
let bar_y = 200.0

let () =
  run { sound = None; playing = false } @@ fun ~io state ->
  let sound =
    match state.sound with Some s -> s | None -> Sound.init ~io Assets.studiokolomna_open_sky_promotional_289410
  in

  let playing =
    if Input.is_down ~io `space then not state.playing else state.playing
  in

  let playing = if playing then Sound.play ~io sound else playing in

  let duration = Sound.duration sound in
  let current = Sound.current_time sound in
  let progress = if duration > 0.0 then current /. duration else 0.0 in

  Box.fill ~io ~color:Color.black (Window.box ~io);

  let label =
    if playing then "Playing  [SPACE to pause]" else "Paused   [SPACE to play]"
  in
  Text.draw ~io ~color:Color.white ~at:(Point.v bar_x 140.0) label;

  let time_str = Printf.sprintf "%.1f / %.1f s" current duration in
  Text.draw ~io ~color:Color.white ~at:(Point.v bar_x 170.0) time_str;

  let track_box = Box.v (Point.v bar_x bar_y) (Size.v bar_w bar_h) in
  Box.fill ~io ~color:(Color.rgb 60 60 60) track_box;

  let fill_w = bar_w *. progress in
  if fill_w > 0.0 then
    Box.fill ~io ~color:Color.cyan
      (Box.v (Point.v bar_x bar_y) (Size.v fill_w bar_h));

  Box.draw ~io ~color:Color.white track_box;

  { sound = Some sound; playing }
