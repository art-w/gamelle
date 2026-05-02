open! Brr
open Gamelle_common
open Jsoo
open Brr_webaudio
open Brr_io
open Audio
open Fut.Syntax

let dont_wait f = Fut.await f Fun.id

let get_ok r =
  match r with
  | Ok a -> a
  | Error e ->
      Console.error [ e ];
      exit 1

let ( let*? ) fut func = Fut.bind fut (fun v -> func (get_ok v))

let mime_type_of binstring =
  let starts_with prefix =
    String.length binstring >= String.length prefix
    && String.sub binstring 0 (String.length prefix) = prefix
  in
  if starts_with "RIFF" then "audio/wav"
  else if starts_with "fLaC" then "audio/flac"
  else if starts_with "OggS" then "audio/ogg"
  else if starts_with "ID3" then "audio/mpeg"
  else if
    String.length binstring >= 2
    && Char.code binstring.[0] = 0xFF
    && Char.code binstring.[1] land 0xE0 = 0xE0
  then "audio/mpeg"
  else "audio/wav"

let blob_url binstring =
  let mime = mime_type_of binstring in
  let img = Bitmap.tarray_of_string binstring in
  let b64 =
    Base64.data_of_binary_jstr @@ Tarray.to_binary_jstr
    @@ Tarray.of_bigarray1 img
  in
  let b64 =
    match Base64.encode b64 with
    | Ok v -> v
    | Error e ->
        Console.(log [ "b64"; e ]);
        failwith "base64 encode"
  in
  Jstr.of_string ("data:" ^ mime ^ ";base64," ^ Jstr.to_string b64)

type data = {
  delayed : (io, Buffer.t Fut.t) Delayed.t;
  mutable buffer : Buffer.t option;
}

let load binstring =
  let s = blob_url binstring in
  let delayed =
    Delayed.make (fun ~io ->
        let audio_ctx = io.backend.audio in
        let node_ctx = Context.as_base audio_ctx in
        let req = Fetch.Request.v s in
        let*? response = Fetch.request req in
        let*? array_buffer =
          response |> Fetch.Response.as_body |> Fetch.Body.array_buffer
        in
        let array_buffer = array_buffer |> Tarray.Buffer.to_jv in
        array_buffer |> Buffer.of_jv
        |> Context.Base.decode_audio_data node_ctx
        |> Fut.map get_ok)
  in
  { delayed; buffer = None }

let ensure_loaded ~io data =
  dont_wait
  @@
  let* buf = Delayed.force ~io data.delayed in
  data.buffer <- Some buf;
  Fut.return ()

let data_duration data =
  match data.buffer with
  | Some buf -> float (Buffer.length buf) /. Buffer.sample_rate buf
  | None -> 0.0

type playing_node = {
  src : Node.Buffer_source.t;
  start_audio_time : float;
  start_offset : float;
}

type status = Idle | Playing of playing_node | Done

type t = {
  data : data;
  mutable status : status;
  mutable elapsed : float;
  mutable last_frame : int;
}

let init ~io data =
  ensure_loaded ~io data;
  { data; status = Idle; elapsed = 0.0; last_frame = -1 }

let duration t = data_duration t.data
let current_time t = t.elapsed
let time_left t = Float.max 0.0 (duration t -. t.elapsed)
let node_ctx_of ~io = Context.as_base io.backend.audio

let make_and_start_source ~io ~offset buffer =
  let node_ctx = node_ctx_of ~io in
  let src = Node.Buffer_source.create node_ctx in
  Node.Buffer_source.set_buffer src (Some buffer);
  let node = Node.Buffer_source.as_node src in
  let dst = node_ctx |> Context.Base.destination |> Node.Destination.as_node in
  Node.connect_node node ~dst;
  Node.Buffer_source.start ~offset src;
  let start_audio_time = Context.Base.current_time node_ctx in
  { src; start_audio_time; start_offset = offset }

let stop_node n = try Node.Buffer_source.stop n.src with _ -> ()

module Playing_set = Weak.Make (struct
  type nonrec t = t

  let equal a b = a == b
  let hash = Hashtbl.hash
end)

let playing_sounds = Playing_set.create 8

let pause_node ~io t node =
  let node_ctx = node_ctx_of ~io in
  let audio_now = Context.Base.current_time node_ctx in
  let elapsed = node.start_offset +. (audio_now -. node.start_audio_time) in
  t.elapsed <- Float.min elapsed (duration t);
  stop_node node;
  t.status <- Idle

let end_frame ~io =
  let current_frame = !(io.event).clock in
  playing_sounds
  |> Playing_set.iter begin fun t ->
      match t.status with
      | Playing node when t.last_frame < current_frame -> pause_node ~io t node
      | _ -> ()
    end

let play ~io t =
  let current_frame = !(io.event).clock in
  let node_ctx = node_ctx_of ~io in
  let audio_now = Context.Base.current_time node_ctx in
  begin match t.status with
  | Playing node ->
      let elapsed = node.start_offset +. (audio_now -. node.start_audio_time) in
      if elapsed >= duration t then (
        t.elapsed <- duration t;
        t.status <- Done)
      else t.elapsed <- elapsed
  | _ -> ()
  end;
  t.last_frame <- current_frame;
  begin match t.status with
  | Idle -> (
      match t.data.buffer with
      | Some buffer ->
          let node = make_and_start_source ~io ~offset:t.elapsed buffer in
          t.status <- Playing node;
          Playing_set.add playing_sounds t
      | None -> ensure_loaded ~io t.data)
  | Playing _ | Done -> ()
  end;
  match t.status with Done -> false | _ -> true

let play_loop ~io t =
  if not (play ~io t) then (
    t.status <- Idle;
    t.elapsed <- 0.0;
    ignore (play ~io t))

let play_until_end ~io data =
  dont_wait
  @@
  let* buf = Delayed.force ~io data.delayed in
  data.buffer <- Some buf;
  let node_ctx = node_ctx_of ~io in
  let src = Node.Buffer_source.create node_ctx in
  Node.Buffer_source.set_buffer src (Some buf);
  let node = Node.Buffer_source.as_node src in
  let dst = node_ctx |> Context.Base.destination |> Node.Destination.as_node in
  Node.connect_node node ~dst;
  Fut.return @@ Node.Buffer_source.start src

let current_music : (data * Node.Buffer_source.t) option ref = ref None

let start_music ~io data =
  dont_wait
  @@
  let* buf = Delayed.force ~io data.delayed in
  data.buffer <- Some buf;
  let node_ctx = node_ctx_of ~io in
  let src = Node.Buffer_source.create node_ctx in
  Node.Buffer_source.set_loop src true;
  Node.Buffer_source.set_buffer src (Some buf);
  let node = Node.Buffer_source.as_node src in
  let dst = node_ctx |> Context.Base.destination |> Node.Destination.as_node in
  Node.connect_node node ~dst;
  Node.Buffer_source.start src;
  current_music := Some (data, src);
  Fut.return ()

let stop_music ~io:_ =
  match !current_music with
  | Some (_, src) -> (
      current_music := None;
      try Node.Buffer_source.stop src with _ -> ())
  | None -> ()

let play_music ~io data =
  match !current_music with
  | Some (music, _) when data == music -> ()
  | _ ->
      stop_music ~io;
      start_music ~io data
