module Tsdl = Tsdl.Sdl
module Tsdl_image = Tsdl_image.Image
open Utils

let ( let& ) x f = match x with Error (`Msg m) -> failwith m | Ok x -> f x

type loader = Raw of string | Parts of string * string * string list

let extension_loader ~sysname ~ext =
  match ext with
  | ".ttf" -> Some (Raw "Gamelle.Font.load")
  | ".png" | ".jpeg" | ".jpg" ->
      let& img = Tsdl_image.load sysname in
      let w, h = Tsdl.get_surface_size img in
      Tsdl.free_surface img;
      let raw = Printf.sprintf "Gamelle.Bitmap.load ~w:%i ~h:%i" w h in
      let parts = sysname ^ ".parts" in
      if Sys.file_exists parts then
        let parts =
          file_contents parts |> String.split_on_char '\n'
          |> List.filter (( <> ) "")
          |> List.rev
        in
        let parts =
          List.map
            (fun line ->
              match String.split_on_char ' ' line with
              | [ x; y; w; h ] ->
                  Printf.sprintf "~x:(%s) ~y:(%s) ~w:(%s) ~h:(%s)" x y w h
              | _ -> line)
            parts
        in
        Some (Parts (raw, "Gamelle.Bitmap.sub", parts))
      else Some (Raw raw)
  | ".mp3" | ".wav" -> Some (Raw "Gamelle.Sound.load")
  | _ -> Some (Raw "Fun.id")

let rec traverse sysname =
  let name =
    normalize_name @@ Filename.remove_extension @@ Filename.basename sysname
  in
  if is_directory sysname then (
    let name = String.capitalize_ascii name in
    Format.printf "module %s = struct@." name;
    let lst = Sys.readdir sysname in
    Array.iter (fun child -> traverse (Filename.concat sysname child)) lst;
    Format.printf "end@.")
  else if is_regular_file sysname then
    let ext = Filename.extension sysname in
    match extension_loader ~sysname ~ext with
    | Some (Raw loader) ->
        Format.printf "  let %s = %s %S@." name loader (file_contents sysname)
    | Some (Parts (loader, extract, parts)) ->
        Format.printf "  let %s =\n" name;
        Format.printf "    let raw = %s %S in\n" loader (file_contents sysname);
        Format.printf "    [|\n";
        List.iter (Format.printf "      %s raw %s;\n" extract) parts;
        Format.printf "    |]@."
    | None -> ()

let run () =
  let _ = Tsdl_image.init Tsdl_image.Init.(jpg + png) in
  let cwd = Sys.getcwd () in
  Array.iter traverse (Sys.readdir cwd);
  Tsdl_image.quit ()
