open Gamelle

let hm = Anim.scale 0.8 @@ Anim.v 2.0 (fun t -> t)
let hm' = Anim.scale 0.6 @@ Anim.seq hm hm
let hm'' = Anim.scale 0.3 @@ Anim.seq hm hm'
let () = Format.printf "%f %f@." (Anim.duration hm) (Anim.duration hm')
let hm = Anim.map2 (fun a b -> a +. b) hm'' hm'
let h0, h1 = Anim.cut 1.3 hm
let () = Format.printf "%f %f@." (Anim.duration h0) (Anim.duration h1)
let hm2 = Anim.seq h0 h1

let () =
  let dt = 0.01 in
  let _ =
    List.fold_left
      (fun (hm, h) () ->
        let hm = Anim.update ~dt hm in
        let h = Anim.update ~dt h in
        let v = Anim.get hm in
        let v' = Anim.get h in
        if v <> v' then Format.printf "%f %f@." v v';
        (hm, h))
      (hm, hm2)
      (List.init 1000 (fun _ -> ()))
  in
  ()

type t = { pos : Point.t Anim.t; color : Color.t Anim.t; radius : float Anim.t }

let anim_radius =
  Anim.v 0.2 (fun t ->
      let t = Ease.in_out_back t in
      15.0 +. (t *. 20.0))

let initial_state =
  {
    pos = Anim.const 0.0 (Point.v 100.0 100.0);
    color = Anim.const 1.0 Color.white;
    radius = Anim.(scale 2.0 (anim_radius >> rev anim_radius));
  }

let anim_id anim =
  let d = Anim.duration anim in
  let t = Random.float d in
  let c0, c1 = Anim.cut t anim in
  Anim.seq c0 c1

type particles = (io:io -> unit) Anim.t list

let new_particle origin =
  let target =
    Vec.v (Random.float 100.0 -. 50.0) (50.0 +. Random.float 200.0)
  in
  let alpha = Anim.v 1.0 @@ fun t -> 1.0 -. t in
  let pos = Anim.v 1.0 @@ fun t -> Point.lerp t origin Vec.(origin + target) in
  let radius = Anim.v 1.0 @@ fun t -> 20.0 -. (t *. 20.0) in
  let circle = Anim.map2 Circle.v pos radius in
  let blue = Random.float 0.1 in
  Anim.scale (0.5 +. Random.float 1.0)
  @@ Anim.map2
       (fun alpha circle ~io ->
         let color = Color.v 1.0 0.5 blue alpha in
         Circle.fill ~io ~color circle)
       alpha circle

let update_particles ~dt ps : particles =
  let ps = List.map (Anim.update ~dt) ps in
  List.filter (fun a -> Anim.duration a > 0.0) ps

let rec loop ~io (particles, { pos; color; radius }) =
  if Input.is_pressed ~io `escape then raise Exit;
  Window.show_cursor ~io true;

  let pos, color, radius =
    if Input.is_up ~io `click_left then
      let pos =
        let mouse = Input.mouse_pos ~io in
        let open Anim in
        let> at = anim_id pos in
        let dist = Vec.norm @@ Vec.(at - mouse) in
        v (dist /. 500.0) (fun t -> Point.lerp (Ease.in_out_cubic t) at mouse)
      in
      let color = Anim.frames 1.0 [| Color.red; Color.green; Color.blue |] in
      let radius = Anim.seq radius initial_state.radius in
      (pos, color, radius)
    else (pos, color, radius)
  in

  let pos = Anim.update ~dt:(dt ~io) pos in
  let color = Anim.update ~dt:(dt ~io) color in
  let radius = Anim.update ~dt:(dt ~io) radius in

  let color_radius = Anim.map2 (fun a b -> (a, b)) color radius in
  let at = Anim.get pos in

  let r' = Anim.get radius in
  let c, r = Anim.get color_radius in
  if abs_float (r -. r') > 0.0001 then Format.printf "r=%f r'=%f@." r r';
  Circle.(fill ~io ~color:c @@ v at r);

  draw_string ~io ~at:Point.zero
  @@ Printf.sprintf "durations: %.3f %.3f" (Anim.duration pos)
       (Anim.duration color);

  let particles = new_particle (Input.mouse_pos ~io) :: particles in
  List.iter (fun draw -> Anim.get draw ~io) particles;
  let particles = update_particles ~dt:(dt ~io) particles in
  next_frame ~io;
  loop ~io (particles, { pos; color; radius })

let () = Gamelle.run (loop ( [], initial_state))
