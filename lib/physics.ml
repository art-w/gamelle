(* Based on https://github.com/twobitcoder101/FlatPhysics-part-24 *)
open Gamelle_backend
open Draw_geometry

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

let shape t = t.shape
let center t = t.pos
let velocity t = t.speed
let rotation t = t.rot
let rot_velocity t = t.rot_speed

let set_center pos t =
  { t with pos; shape = Shape.translate Vec.(pos - t.pos) t.shape }

let set_velocity speed t = { t with speed }
let set_rot_velocity rot_speed t = { t with rot_speed }

let set_rotation rot t =
  { t with rot; shape = Shape.rotate (rot -. t.rot) t.shape }

let add_velocity vec t =
  match t.kind with
  | Immovable -> t
  | _ -> { t with speed = Vec.(t.speed + vec) }

let add_rot_velocity dv t =
  match t.kind with
  | Immovable -> t
  | _ -> { t with rot_speed = t.rot_speed +. dv }

let draw ~io ?color { shape; pos; rot; _ } =
  Shape.draw ~io ?color shape;
  draw_line ~io ?color (Segment.v pos Vec.(pos + polar 10.0 rot))

let fill ~io ?color { shape; _ } = Shape.fill ~io ?color shape

let v ?mass ?inertia ?(restitution = 0.2) ?(kind = Movable) shape =
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
          Shape.rotate dt_rot_speed ~center:pos
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
    let a_rot_speed = -.a.inv_inertia *. Vec.cross ra impulse in
    let b_rot_speed = b.inv_inertia *. Vec.cross rb impulse in
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
        let a_rot_speed = -.a.inv_inertia *. Vec.cross ra impulse in
        let b_rot_speed = b.inv_inertia *. Vec.cross rb impulse in
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

let detect_collisions shapes =
  let had_collision = ref false in
  !shapes
  |> World.iter begin fun i _ ->
      !shapes
      |> World.iter begin fun j _ ->
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
                  shapes := World.add j b !shapes
        end
    end;
  !had_collision

let fix_collisions_map t =
  let shapes = ref t in
  let rec go n = if n > 0 && detect_collisions shapes then go (n - 1) in
  go 10;
  !shapes

let fix_collisions t =
  let shapes = ref World.empty in
  List.iteri (fun i shape -> shapes := World.add i shape !shapes) t;
  let rec go n = if n > 0 && detect_collisions shapes then go (n - 1) in
  go 10;
  List.map snd (World.bindings !shapes)

module TMap = Hashtbl.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash
  let equal = ( == )
end)

type collision_data = (t * t) list TMap.t

let precompute_collisions lst =
  let after = fix_collisions lst in

  let tbl = TMap.create 64 in
  begin
    List.iter2
      begin fun before after ->
        match TMap.find_opt tbl before with
        | None -> TMap.replace tbl before [ (before, after) ]
        | Some lst -> TMap.replace tbl before ((before, after) :: lst)
      end
      lst after
  end;
  tbl

let apply_collisions (data : collision_data) t =
  let lst = TMap.find data t in
  List.find (fun (before, _after) -> before == t) lst |> snd

module
  H
  (* :  sig
  type world
  type closed_world
  type id
  type fixed_world = Leaf of t | Node of world * world

  val register_in_world : world -> t -> world * id
  val close_world : world -> closed_world
  val fix_collisions_world : closed_world -> fixed_world
end  *) =
struct
  type world = Leaf of t | LeafLi of t list | Node of world * world
  type closed_world = world
  type fixed_world = world

  let close_world world = world
  let register_in_world world t = (t :: world, List.length world)

  let fix_collisions_world (world : world) : fixed_world =
    let map = ref World.empty in
    let rec loop i w =
      match w with
      | Leaf t ->
          map := World.add i t !map;
          i + 1
      | LeafLi ts ->
          List.iteri (fun j t -> map := World.add (i + j) t !map) ts;
          i + List.length ts
      | Node (a, b) ->
          let i = loop i a in
          let i = loop i b in
          i
    in
    loop 0 world |> ignore;
    let fixed_map = fix_collisions_map !map in
    let rec loop i w =
      match w with
      | Leaf _t -> (i + 1, Leaf (World.find i fixed_map))
      | LeafLi ts ->
          let n = List.length ts in
          let fixed_ts = List.init n (fun j -> World.find (i + j) fixed_map) in
          (i + n, LeafLi fixed_ts)
      | Node (a, b) ->
          let i, a = loop i a in
          let i, b = loop i b in
          (i, Node (a, b))
    in
    loop 0 world |> snd
end

type 'a app = H.world * (H.fixed_world -> 'a)

let ( let+ ) ((w, f') : 'a app) (f : 'a -> 'b) : 'b =
  let open H in
  let closed_world = close_world w in
  let fixed_world = fix_collisions_world closed_world in
  let a = f' fixed_world in
  f a

let ( and+ ) ((wa, fa) : 'a app) ((wb, fb) : 'b app) =
  let open H in
  let w = H.Node (wa, wb) in
  let f =
   fun fixed_world ->
    match fixed_world with
    | Leaf _ | LeafLi _ ->
        failwith
          "Impossible: fixed world should have the same structure as the world"
    | Node (wa, wb) ->
        let a = fa wa in
        let b = fb wb in
        (a, b)
  in
  (w, f)

let const a =
  ( H.Leaf a,
    function
    | H.Leaf a -> a
    | H.Node _ | H.LeafLi _ ->
        failwith
          "Impossible: fixed world should have the same structure as the world"
  )

let constli a =
  ( H.LeafLi a,
    function
    | H.LeafLi a -> a
    | H.Node _ | H.Leaf _ ->
        failwith
          "Impossible: fixed world should have the same structure as the world"
  )
