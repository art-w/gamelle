(* Based on https://github.com/twobitcoder101/FlatPhysics-part-24 *)
open Gamelle_backend
open Gamelle_common
open Geometry

type kind = Movable | Immovable

type t = {
  pos : Vec.t;
  speed : Vec.t;
  rot : float;
  rot_speed : float;
  shape : Shape.t;
  kind : kind;
  restitution : float;
  inv_mass : float;
  inv_inertia : float;
  static_friction : float;
  dynamic_friction : float;
}

let center t = t.pos

let add_velocity vec t =
  match t.kind with
  | Immovable -> t
  | _ -> { t with speed = Vec.(t.speed + vec) }

let add_rot_velocity dv t =
  match t.kind with
  | Immovable -> t
  | _ -> { t with rot_speed = t.rot_speed +. dv }

let draw ~io { kind; shape; pos; rot; _ } =
  let color, colora =
    match kind with
    | Immovable -> (Color.v 0.0 1.0 1.0 1.0, Color.v 0.0 1.0 1.0 0.3)
    | Movable -> (Color.v 1.0 1.0 0.0 1.0, Color.v 1.0 1.0 0.0 0.3)
  in
  Shape.draw ~io ~color shape;
  Shape.fill ~io ~color:colora shape;
  draw_line ~io ~color:colora (Segment.v pos Vec.(pos + polar 10.0 rot))

let make ?mass ?inertia ?(restitution = 0.2) ?(kind = Movable) shape =
  let pos = Shape.center shape in
  let mass =
    match mass with
    | None -> abs_float (Shape.signed_area shape /. 1000.0)
    | Some m -> m
  in
  let inertia = match inertia with None -> 1000.0 *. mass | Some i -> i in

  {
    kind;
    pos;
    shape;
    speed = Vec.zero;
    rot = 0.0;
    rot_speed = 0.0;
    restitution;
    inv_mass = (if kind = Movable && mass > 0.0 then 1.0 /. mass else 0.0);
    inv_inertia =
      (if kind = Movable && inertia > 0.0 then 1.0 /. inertia else 0.0);
    static_friction = 0.6;
    dynamic_friction = 0.4;
  }

let update ~dt p =
  match p.kind with
  | Immovable -> p
  | Movable ->
      let dt_speed = Vec.(dt * p.speed) in
      let pos = Vec.(p.pos + dt_speed) in
      let dt_rot_speed = dt *. p.rot_speed in
      {
        p with
        pos;
        rot = p.rot +. dt_rot_speed;
        speed = Vec.(0.99 * p.speed);
        rot_speed = 0.99 *. p.rot_speed;
        shape =
          Shape.rotate ~angle:dt_rot_speed ~center:pos
          @@ Shape.translate dt_speed p.shape;
      }

let mass t =
  if t.kind = Immovable || t.inv_mass = 0.0 then 0.0 else 1.0 /. t.inv_mass

let separate_bodies a b collision =
  let wa, wb =
    let a_mass = mass a in
    let b_mass = mass b in
    match (a_mass = 0.0, b_mass = 0.0) with
    | true, true -> (0.0, 0.0)
    | true, false -> (0.0, 1.0)
    | false, true -> (1.0, 0.0)
    | false, false ->
        let total_mass = a_mass +. b_mass in
        (b_mass /. total_mass, a_mass /. total_mass)
  in
  let a_collision = Vec.(wa * collision) in
  let b_collision = Vec.(-.wb * collision) in
  let a =
    {
      a with
      pos = Vec.(a.pos + a_collision);
      shape = Shape.translate a_collision a.shape;
    }
  in
  let b =
    {
      b with
      pos = Vec.(b.pos + b_collision);
      shape = Shape.translate b_collision b.shape;
    }
  in
  (a, b)

let separate a b =
  match Shape.separation_axis a.shape b.shape with
  | None -> None
  | Some collision ->
      let a, b = separate_bodies a b collision in
      let normal = Vec.(-1.0 * unit collision) in
      Some (normal, a, b)

let cross p0 p1 =
  let a, b = Vec.to_tuple p0 in
  let c, d = Vec.to_tuple p1 in
  (a *. d) -. (b *. c)

let contact_collision ~n ~normal ~e ~static_friction ~dynamic_friction a b
    contact_point =
  let ra = Vec.(contact_point - a.pos) in
  let rb = Vec.(contact_point - b.pos) in
  let ra_perp = Vec.(ortho ra) in
  let rb_perp = Vec.(ortho rb) in
  let a_angular_speed = Vec.(a.rot_speed * ra_perp) in
  let b_angular_speed = Vec.(b.rot_speed * rb_perp) in
  let rel_velocity =
    Vec.(b.speed + b_angular_speed - (a.speed + a_angular_speed))
  in
  let velocity_magn = Vec.(dot rel_velocity normal) in
  if velocity_magn >= 0.0 then None
  else
    let ra_perp_dot_n = Vec.dot ra_perp normal in
    let rb_perp_dot_n = Vec.dot rb_perp normal in
    let j = -.(1.0 +. e) *. velocity_magn in
    let denom =
      a.inv_mass +. b.inv_mass
      +. (ra_perp_dot_n *. ra_perp_dot_n *. a.inv_inertia)
      +. (rb_perp_dot_n *. rb_perp_dot_n *. b.inv_inertia)
    in
    let j = j /. denom in
    let j = j /. n in
    let impulse = Vec.(j * normal) in
    let a_rot_speed = -.a.inv_inertia *. cross ra impulse in
    let b_rot_speed = b.inv_inertia *. cross rb impulse in
    let friction_impulse, a_friction_rot, b_friction_rot =
      let tangeant = Vec.(rel_velocity - (dot rel_velocity normal * normal)) in
      if Vec.(norm2 tangeant) < 0.1 then (Vec.zero, 0.0, 0.0)
      else
        let tangeant = Vec.unit tangeant in
        let ra_perp_dot_t = Vec.dot ra_perp tangeant in
        let rb_perp_dot_t = Vec.dot rb_perp tangeant in
        let denom =
          a.inv_mass +. b.inv_mass
          +. (ra_perp_dot_t *. ra_perp_dot_t *. a.inv_inertia)
          +. (rb_perp_dot_t *. rb_perp_dot_t *. b.inv_inertia)
        in
        let jt = -.Vec.dot rel_velocity tangeant in
        let jt = jt /. denom in
        let jt = jt /. n in
        let jt =
          if abs_float jt <= j *. static_friction then jt
          else -.j *. dynamic_friction
        in
        let impulse = Vec.(jt * tangeant) in
        let a_rot_speed = -.a.inv_inertia *. cross ra impulse in
        let b_rot_speed = b.inv_inertia *. cross rb impulse in
        (impulse, a_rot_speed, b_rot_speed)
    in
    Some
      ( Vec.(impulse + friction_impulse),
        a_rot_speed +. a_friction_rot,
        b_rot_speed +. b_friction_rot )

let collision_react ~normal a b =
  let _, contact_points = Shape.contact_points a.shape b.shape in
  let static_friction = (a.static_friction +. b.static_friction) /. 2.0 in
  let dynamic_friction = (a.dynamic_friction +. b.dynamic_friction) /. 2.0 in
  let n = float (List.length contact_points) in
  let e = min a.restitution b.restitution in
  let impulse, a_rot_speed, b_rot_speed =
    List.fold_left
      (fun (i, ar, br) (i', ar', br') -> (Vec.(i + i'), ar +. ar', br +. br'))
      (Vec.zero, 0., 0.)
    @@ List.filter_map
         (contact_collision ~n ~normal ~e ~static_friction ~dynamic_friction a b)
         contact_points
  in
  let a_speed = Vec.(-.a.inv_mass * impulse) in
  let b_speed = Vec.(b.inv_mass * impulse) in
  let a =
    {
      a with
      speed = Vec.(a.speed + a_speed);
      rot_speed = a.rot_speed +. a_rot_speed;
    }
  in
  let b =
    {
      b with
      speed = Vec.(b.speed + b_speed);
      rot_speed = b.rot_speed +. b_rot_speed;
    }
  in
  (a, b)

module World = Map.Make (Int)

let fix_collisions shapes =
  let had_collision = ref false in
  World.iter
    (fun i _ ->
      World.iter
        (fun j _ ->
          if i >= j then ()
          else
            let a = World.find i !shapes in
            let b = World.find j !shapes in
            if a.kind = Immovable && b.kind = Immovable then ()
            else
              match separate a b with
              | None -> ()
              | Some (normal, a, b) ->
                  had_collision := true;
                  let a, b = collision_react ~normal a b in
                  shapes := World.add i a !shapes;
                  shapes := World.add j b !shapes)
        !shapes)
    !shapes;
  !had_collision

let fix_collisions t =
  let shapes = ref World.empty in
  List.iteri (fun i shape -> shapes := World.add i shape !shapes) t;
  let rec go n = if n > 0 && fix_collisions shapes then go (n - 1) in
  go 10;
  List.map snd (World.bindings !shapes)
