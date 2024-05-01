open Gamelle

type player = Circle | Cross

type cell = player option

type board =
  { left_top: cell
  ; center_top: cell
  ; right_top: cell
  ; left_center: cell
  ; center_center: cell
  ; right_center: cell
  ; left_bottom: cell
  ; center_bottom: cell
  ; right_bottom: cell }

let cell_of_coord board x y =
  match (x, y) with
  | 0, 0 ->
      board.left_top
  | 0, 1 ->
      board.left_center
  | 0, 2 ->
      board.left_bottom
  | 1, 0 ->
      board.center_top
  | 1, 1 ->
      board.center_center
  | 1, 2 ->
      board.center_bottom
  | 2, 0 ->
      board.right_top
  | 2, 1 ->
      board.right_center
  | 2, 2 ->
      board.right_bottom
  | _ ->
      invalid_arg (Printf.sprintf "cell_of_coord: invalid coordinate %i %i" x y)

let update_cell_of_coord board new_cell x y =
  match (x, y) with
  | 0, 0 ->
      {board with left_top= new_cell}
  | 0, 1 ->
      {board with left_center= new_cell}
  | 0, 2 ->
      {board with left_bottom= new_cell}
  | 1, 0 ->
      {board with center_top= new_cell}
  | 1, 1 ->
      {board with center_center= new_cell}
  | 1, 2 ->
      {board with center_bottom= new_cell}
  | 2, 0 ->
      {board with right_top= new_cell}
  | 2, 1 ->
      {board with right_center= new_cell}
  | 2, 2 ->
      {board with right_bottom= new_cell}
  | _ ->
      invalid_arg
        (Printf.sprintf "update_cell_of_coord: invalid coordinate %i %i" x y)

type state = {board: board; player: player}

let empty_board =
  { left_top= None
  ; center_top= None
  ; right_top= None
  ; left_center= None
  ; center_center= None
  ; right_center= None
  ; left_bottom= None
  ; center_bottom= None
  ; right_bottom= None }

let state = {board= empty_board; player= Circle}

let victory board =
  match board with
  (* rows *)
  | {left_top= Some p1; center_top= Some p2; right_top= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | {left_center= Some p1; center_center= Some p2; right_center= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | {left_bottom= Some p1; center_bottom= Some p2; right_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  (* columns *)
  | {left_top= Some p1; left_center= Some p2; left_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | {center_top= Some p1; center_center= Some p2; center_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | {right_top= Some p1; right_center= Some p2; right_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  (* diagonals *)
  | {left_top= Some p1; center_center= Some p2; right_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | {right_top= Some p1; center_center= Some p2; left_bottom= Some p3; _}
    when p1 = p2 && p2 = p3 ->
      Some p1
  | _ ->
      None

let other_player player = match player with Circle -> Cross | Cross -> Circle

let cell_size = 64.

let size = cell_size *. 3.

let draw_background ~io () =
  let color = Color.white in
  Box.fill ~io ~color (Box.v (Point.v 0. 0.) (Size.v size size))

let draw_grid ~io () =
  let color = Color.black in
  Segment.(draw ~io ~color (v (Point.v cell_size 0.) (Point.v cell_size size))) ;
  Segment.(
    draw ~io ~color
      (v (Point.v (cell_size *. 2.) 0.) (Point.v (cell_size *. 2.) size)) ) ;
  Segment.(draw ~io ~color (v (Point.v 0. cell_size) (Point.v size cell_size))) ;
  Segment.(
    draw ~io ~color
      (v (Point.v 0. (cell_size *. 2.)) (Point.v size (cell_size *. 2.))) )

let draw_cell ~io cell p =
  match cell with
  | Some Circle ->
      draw ~io Assets.circle ~at:p
  | Some Cross ->
      draw ~io Assets.cross ~at:p
  | None ->
      ()

let draw_board ~io
    ({ left_top
     ; center_top
     ; right_top
     ; left_center
     ; center_center
     ; right_center
     ; left_bottom
     ; center_bottom
     ; right_bottom } :
      board ) =
  draw_cell ~io left_top (Point.v 0. 0.) ;
  draw_cell ~io center_top (Point.v cell_size 0.) ;
  draw_cell ~io right_top (Point.v (cell_size *. 2.) 0.) ;
  draw_cell ~io left_center (Point.v 0. cell_size) ;
  draw_cell ~io center_center (Point.v cell_size cell_size) ;
  draw_cell ~io right_center (Point.v (cell_size *. 2.) cell_size) ;
  draw_cell ~io left_bottom (Point.v 0. (cell_size *. 2.)) ;
  draw_cell ~io center_bottom (Point.v cell_size (cell_size *. 2.)) ;
  draw_cell ~io right_bottom (Point.v (cell_size *. 2.) (cell_size *. 2.))

let failsound = Assets.Audio.confirm1

let () =
  run state
  @@ fun ~io state ->
  let {board; player} = state in
  let state =
    match victory board with
    | Some Circle ->
        print_endline "Circle won" ; exit 0
    | Some Cross ->
        print_endline "Cross won" ; exit 0
    | None ->
        if Input.is_pressed ~io `click_left then
          let x, y = Vec.to_tuple @@ Input.mouse_pos ~io in
          (* first column *)
          let cell_x = Int.of_float (floor x /. 64.)
          and cell_y = Int.of_float (floor (y /. 64.)) in
          if cell_x < 0 || cell_x >= 3 || cell_y < 0 || cell_y >= 3 then (
            Sound.play ~io failsound ; state )
          else if Option.is_some (cell_of_coord board cell_x cell_y) then state
          else
            let board =
              update_cell_of_coord board (Some player) cell_x cell_y
            in
            let player = other_player player in
            {player; board}
        else state
  in
  Window.show_cursor ~io true ;
  draw_background ~io () ;
  draw_board ~io state.board ;
  draw_grid ~io () ;
  state
