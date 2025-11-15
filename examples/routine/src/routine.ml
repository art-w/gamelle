open Gamelle

let bubble_size = 5.
let random_vec () = Vec.v (Random.float 2. -. 1.) (Random.float 2. -. 1.)

let bubble ~io pos ~next_frame _is_colliding =
  let rec loop pos =
    Format.printf "%a@." Vec.pp pos;
    let is_colliding = next_frame pos in
    let color = if is_colliding then Color.red else Color.green in
    let circle = Circle.v pos bubble_size in
    Circle.draw ~io ~color circle;
    if
      Input.is_down ~io `click_right
      && Circle.intersect (Circle.v (Input.mouse_pos ~io) 0.) circle
    then ()
    else loop Vec.(pos + random_vec ())
  in
  loop pos

let rec loop ~io bubbles =
  let bubbles =
    (* create a bubble on left click *)
    if Input.is_down ~io `click_left then
      (false, Routine.start (bubble ~io (Input.mouse_pos ~io))) :: bubbles
    else bubbles
  in
  let bubbles =
    bubbles
    |> List.filter_map begin fun (was_colliding, bubble) ->
        let bubble = Routine.tick bubble was_colliding in
        match bubble with
        | Start _ -> assert false
        | Finished () -> None
        | Running (pos, _) -> Some (pos, bubble)
      end
  in
  let bubbles =
    bubbles
    |> List.map begin fun (pos, bubble) ->
        let is_colliding =
          bubbles
          |> List.exists begin fun (pos', _) ->
              pos' != pos
              && Circle.intersect (Circle.v pos bubble_size)
                   (Circle.v pos' bubble_size)
            end
        in
        (is_colliding, bubble)
      end
  in
  next_frame ~io;
  loop ~io bubbles

let () = Gamelle.run_no_loop (loop [])
