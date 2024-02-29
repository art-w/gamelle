open Brr
open Common

type t = { name : string }

let uid = ref 0

let gen () =
  let u = !uid in
  uid := u + 1;
  u

let load binstring =
  let name = "GamelleFont" ^ string_of_int (gen ()) in
  Console.(log [ "font loading..."; String.length binstring ]);
  let arr = Bitmap.tarray_of_string binstring in
  let fontface_class = Jv.get Jv.global "FontFace" in
  let ttf = Jv.new' fontface_class [| Jv.of_string name; Tarray.to_jv arr |] in
  Console.(log [ "font face!!"; ttf ]);
  let _loading = Jv.call ttf "load" [||] in
  (* TODO: [then] callback! *)
  Console.(log [ "font face loading"; _loading ]);
  let _ =
    Jv.call _loading "then"
      [|
        Jv.callback ~arity:1 (fun fontface ->
            Console.(log [ "font loaded!!" ]);
            let fonts = Jv.get (Document.to_jv G.document) "fonts" in
            let _res = Jv.call fonts "add" [| fontface |] in
            Console.(log [ "font added!!"; _res ]));
        Jv.callback ~arity:1 (fun e ->
            Console.(log [ "font error loaded!!"; e ]));
      |]
  in
  { name }

let draw _ _ _ = failwith "draw"

let draw_at font ~size text (x, y) =
  C.set_font (render ())
    (Jstr.of_string (string_of_int size ^ "px " ^ font.name));
  (* TODO *)
  let text = Jstr.of_string text in
  let metrics = C.measure_text (render ()) text in
  C.fill_text (render ()) text ~x
    ~y:(y +. C.Text_metrics.font_bounding_box_ascent metrics)
