open! Brr
module C = Brr_canvas.C2d

type t = { image : C.image_src; backend : Jv.t; mutable error : bool }

let class_image = Jv.get Jv.global "Image"
let new_image () = Jv.new' class_image [||]

let tarray_of_string binstring =
  let len = String.length binstring in
  let arr = Tarray.create Tarray.Uint8 len in
  for i = 0 to len - 1 do
    Tarray.set arr i (Char.code binstring.[i])
  done;
  arr

let load binstring =
  let img = tarray_of_string binstring in

  let b64 = Base64.data_of_binary_jstr @@ Tarray.to_binary_jstr img in
  let b64 =
    match Base64.encode b64 with
    | Ok v -> v
    | Error e ->
        Console.(log [ "b64"; e ]);
        failwith "base64 encode"
  in
  let blob_url =
    Jstr.of_string ("data:image/png;base64," ^ Jstr.to_string b64)
  in
  let image = new_image () in
  let img =
    { image = C.image_src_of_jv image; backend = image; error = false }
  in
  (* NOT CALLED
     Jv.set img "onload" (Jv.callback ~arity:0 (fun _v -> print_endline "onload!!!"));
  *)
  Jv.set image "onerror" (Jv.callback ~arity:1 (fun _v -> img.error <- true));
  Jv.set image "src" (Jv.of_jstr blob_url);
  img

let is_complete t = (not t.error) && Jv.to_bool (Jv.get t.backend "complete")

let draw ~view:_ ~ctx t ~x ~y =
  if is_complete t then C.draw_image ctx t.image ~x ~y

let rotate _ t = t
let scale _ t = t
let sub t _ _ _ _ = t
