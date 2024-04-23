open! Brr
open Gamelle_common
open Jsoo
open Brr_webaudio
open Brr_io
open Audio

type sound = Jstr.t

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

let load binstring = blob_url binstring

let play ~io s =
  dont_wait
  @@
  let audio_ctx = io.backend.audio in
  let node_ctx = Context.as_base audio_ctx in
  let req = Fetch.Request.v s in
  let*? response = Fetch.request req in
  let*? array_buffer =
    response |> Fetch.Response.as_body |> Fetch.Body.array_buffer
  in
  let array_buffer = array_buffer |> Tarray.Buffer.to_jv in
  let*? buffer =
    array_buffer |> Audio.Buffer.of_jv
    |> Context.Base.decode_audio_data node_ctx
  in
  let src = Node.Buffer_source.create node_ctx in
  Node.Buffer_source.set_buffer src (Some buffer);
  let node = Node.Buffer_source.as_node src in
  let dst = node_ctx |> Context.Base.destination |> Node.Destination.as_node in
  Node.connect_node node ~dst;
  Fut.return @@ Node.Buffer_source.start src

type music = unit

let load_music _ = ()
let play_music ~io:_ _ = ()
