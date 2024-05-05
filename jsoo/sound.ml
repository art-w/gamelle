open! Brr
open Gamelle_common
open Jsoo
open Brr_webaudio
open Brr_io
open Audio
open Fut.Syntax

type t = (io, Audio.Buffer.t Fut.t) Delayed.t

let dont_wait f = Fut.await f Fun.id

let get_ok r =
  match r with
  | Ok a -> a
  | Error e ->
      Console.error [ e ];
      exit 1

let ( let*? ) fut func = Fut.bind fut (fun v -> func (get_ok v))

let await_or_error fut =
  Fut.await fut (fun e -> match e with Ok () -> () | e -> Console.log [ e ])

let blob_url binstring =
  let img = Bitmap.tarray_of_string binstring in
  let b64 = Base64.data_of_binary_jstr @@ Tarray.to_binary_jstr img in
  let b64 =
    match Base64.encode b64 with
    | Ok v -> v
    | Error e ->
        Console.(log [ "b64"; e ]);
        failwith "base64 encode"
  in
  Jstr.of_string ("data:audio/wav;base64," ^ Jstr.to_string b64)

let load binstring =
  let s = blob_url binstring in
  Delayed.make (fun ~io ->
      let audio_ctx = io.backend.audio in
      let node_ctx = Context.as_base audio_ctx in
      let req = Fetch.Request.v s in
      let*? response = Fetch.request req in
      let*? array_buffer =
        response |> Fetch.Response.as_body |> Fetch.Body.array_buffer
      in
      let array_buffer = array_buffer |> Tarray.Buffer.to_jv in
      array_buffer |> Audio.Buffer.of_jv
      |> Context.Base.decode_audio_data node_ctx
      |> Fut.map get_ok)

let play_source ~io ~loop s =
  let audio_ctx = io.backend.audio in
  let node_ctx = Context.as_base audio_ctx in
  let src = Node.Buffer_source.create node_ctx in
  Node.Buffer_source.set_loop src loop;
  let () =
    dont_wait
    @@
    let* buffer = Delayed.force ~io s in
    Node.Buffer_source.set_buffer src (Some buffer);
    let node = Node.Buffer_source.as_node src in
    let dst =
      node_ctx |> Context.Base.destination |> Node.Destination.as_node
    in
    Node.connect_node node ~dst;
    Fut.return @@ Node.Buffer_source.start src
  in
  src

let play ~io s = ignore (play_source ~io ~loop:false s)
let current_music = ref None

let play_music ~io s =
  let src = play_source ~io ~loop:true s in
  current_music := Some (s, src)

let stop_music ~io:_ =
  match !current_music with
  | Some (_, src) ->
      current_music := None;
      Node.Buffer_source.stop src
  | None -> ()

let play_music ~io s =
  match !current_music with
  | Some (music, _) when s == music -> ()
  | _ ->
      stop_music ~io;
      play_music ~io s
