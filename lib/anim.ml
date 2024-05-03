module Chunk = struct
  type 'a t = {
    elapsed : float;
    duration : float;
    speed : float;
    sample : float -> 'a;
  }

  let v duration sample =
    assert (duration >= 0.0);
    let speed = if duration = 0.0 then 1.0 else duration in
    { elapsed = 0.0; duration; speed; sample }

  let duration c = c.duration -. c.elapsed
  let last c = c.sample (c.duration /. c.speed)

  let get c =
    (* let t = c.elapsed /. c.duration in *)
    c.sample (c.elapsed /. c.speed)

  let update ~dt c =
    let elapsed = c.elapsed +. dt in
    if elapsed <= c.duration then Ok { c with elapsed }
    else Error (elapsed -. c.duration)

  let cut t c =
    let at = t +. c.elapsed in
    if at > c.duration then Error c
    else
      let c0 = { c with duration = at } in
      let c1 = { c with elapsed = at } in
      Ok (c0, c1)

  let rev c =
    {
      elapsed = 0.0;
      duration = duration c;
      speed = c.speed;
      sample = (fun t -> c.sample (1.0 -. t));
    }

  let scale f c =
    {
      elapsed = f *. c.elapsed;
      duration = f *. c.duration;
      speed = f *. c.speed;
      sample = c.sample;
    }

  let map f c = { c with sample = (fun t -> f (c.sample t)) }

  let map2 fn a b =
    if abs_float (duration a -. duration b) > 0.0001 then
      invalid_arg
        (Printf.sprintf "Chunk.map2: duration %f <> %f@." (duration a)
           (duration b));
    {
      a with
      sample =
        (fun at ->
          let bt = ((at *. a.speed) -. a.elapsed +. b.elapsed) /. b.speed in
          fn (a.sample at) (b.sample bt));
    }
end

type 'a t = { duration : float; chunks : 'a Chunk.t list; last : 'a Lazy.t }

let of_chunk duration chunk =
  { duration; chunks = [ chunk ]; last = lazy (Chunk.last chunk) }

let of_chunks chunks =
  let duration =
    List.fold_left (fun acc c -> acc +. Chunk.duration c) 0.0 chunks
  in
  let last =
    match List.rev chunks with
    | [] -> invalid_arg "of_chunks: empty list"
    | c :: _ -> lazy (Chunk.last c)
  in
  { duration; chunks; last }

let v duration sample = of_chunk duration (Chunk.v duration sample)
let const duration cst = v duration (fun _ -> cst)
let empty last = { duration = 0.0; chunks = []; last }
let duration t = t.duration
let last t = Lazy.force t.last
let get t = match t.chunks with [] -> last t | c :: _ -> Chunk.get c

let rec update ~dt t =
  match t.chunks with
  | [] -> t
  | c :: cs -> (
      match Chunk.update ~dt c with
      | Ok c -> { t with chunks = c :: cs }
      | Error dt -> update ~dt { t with chunks = cs })

let update ~dt t =
  let t = update ~dt t in
  { t with duration = max 0.0 (t.duration -. dt) }

let rev t =
  match t.chunks with
  | [] -> t
  | first :: _ ->
      let last = lazy (Chunk.get first) in
      let chunks = List.rev_map Chunk.rev t.chunks in
      { t with chunks; last }

let seq a b =
  {
    duration = a.duration +. b.duration;
    chunks = List.rev_append (List.rev a.chunks) b.chunks;
    last = b.last;
  }

let ( >> ) a b = seq a b

let continue a fn =
  let b = fn (last a) in
  seq a b

let ( >>- ) a fn = continue a fn
let ( let> ) = ( >>- )

let frames duration arr =
  v duration @@ fun t ->
  let n = Array.length arr in
  arr.(min (n - 1) (int_of_float (floor (t *. float n))))

let cut t anim =
  let rec cut t acc = function
    | [] -> (anim, empty anim.last)
    | c :: cs -> (
        match Chunk.cut t c with
        | Error c -> cut (t -. Chunk.duration c) (c :: acc) cs
        | Ok (c0, c1) ->
            let left = of_chunks (List.rev (c0 :: acc)) in
            let right = of_chunks (c1 :: cs) in
            (left, right))
  in
  cut t [] anim.chunks

let scale f anim =
  {
    duration = f *. anim.duration;
    chunks = List.map (Chunk.scale f) anim.chunks;
    last = anim.last;
  }

let map f anim =
  {
    duration = anim.duration;
    chunks = List.map (Chunk.map f) anim.chunks;
    last = Lazy.map f anim.last;
  }

let map2 f a b =
  let rec map2 a_chunks b_chunks =
    match (a_chunks, b_chunks) with
    | [], [] -> []
    | [], _ -> List.map (Chunk.map (fun b -> f (Lazy.force a.last) b)) b_chunks
    | _, [] -> List.map (Chunk.map (fun a -> f a (Lazy.force b.last))) a_chunks
    | a_chunk :: a_chunks, b_chunk :: b_chunks ->
        if Chunk.duration a_chunk = Chunk.duration b_chunk then
          Chunk.map2 f a_chunk b_chunk :: map2 a_chunks b_chunks
        else if Chunk.duration a_chunk < Chunk.duration b_chunk then
          let b_chunk, b_rest =
            Result.get_ok @@ Chunk.cut (Chunk.duration a_chunk) b_chunk
          in
          Chunk.map2 f a_chunk b_chunk :: map2 a_chunks (b_rest :: b_chunks)
        else
          let a_chunk, a_rest =
            Result.get_ok @@ Chunk.cut (Chunk.duration b_chunk) a_chunk
          in
          Chunk.map2 f a_chunk b_chunk :: map2 (a_rest :: a_chunks) b_chunks
  in
  match map2 a.chunks b.chunks with
  | [] -> empty (lazy (f (Lazy.force a.last) (Lazy.force b.last)))
  | chunks -> of_chunks chunks
