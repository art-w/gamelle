open! Brr

type sound = Jv.t

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
  Jstr.of_string ("data:image/png;base64," ^ Jstr.to_string b64)

let load binstring =
  let audio_class = Jv.get Jv.global "Audio" in
  let audio = Jv.new' audio_class [||] in
  Jv.set audio "src" (Jv.of_jstr (blob_url binstring));
  audio

let play t =
  let _ = Jv.call t "play" [||] in
  ()

type music = unit

let load_music _ = ()
let play_music _ = ()
