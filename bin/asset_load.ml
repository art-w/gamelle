module Tsdl = Tsdl.Sdl
module Tsdl_image = Tsdl_image.Image
open Utils

let ( let& ) x f = match x with Error (`Msg m) -> failwith m | Ok x -> f x

type loader = Raw of string | Parts of string * string * string list

let extension_loader ~sysname ~basename ~ext =
  match (basename, ext) with
  | _, "Ttf" -> Some (Raw "Gamelle.Font.load")
  | _, ("Png" | "Jpeg" | "Jpg") ->
      let& img = Tsdl_image.load sysname in
      let w, h = Tsdl.get_surface_size img in
      Tsdl.free_surface img;
      let raw = Printf.sprintf "Gamelle.Bitmap.load ~w:%i ~h:%i" w h in
      let parts = sysname ^ ".parts" in
      (* TODO: file_exists after readdir? ... *)
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
  | _, ("Mp3" | "Wav") -> Some (Raw "Gamelle.Sound.load")
  | "assets", _ | "dune", "No_ext" | _, "Parts" -> None
  | _ -> Some (Raw "Fun.id")

let split_file_ext filename =
  let name = normalize_name @@ Filename.remove_extension filename in
  let raw_ext = Filename.extension filename in
  let ext =
    String.capitalize_ascii
    @@
    if raw_ext = "" then "No_ext"
    else if String.starts_with ~prefix:"." raw_ext then
      String.(sub raw_ext 1 (length raw_ext - 1))
    else raw_ext
  in
  (name, ext)

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let list_files k =
  let cwd = Sys.getcwd () in
  Format.printf "(* %S *)@." cwd;
  k (Sys.readdir cwd) cwd

let rec output_file (full_name, basename, loader) =
  if is_directory full_name then (
    Format.printf "@.  (** Generated from %s *)\nmodule %s = struct@." basename
      (String.capitalize_ascii basename);
    gen_ml (Sys.readdir full_name) full_name;
    Format.printf "\nend\n")
  else if is_regular_file full_name then (
    Format.printf "@.  (** Generated from %s *)@." basename;
    match loader with
    | Raw loader ->
        Format.printf "  let %s = %s %S@." basename loader
          (file_contents full_name)
    | Parts (loader, extract, parts) ->
        Format.printf "  let %s =\n" basename;
        Format.printf "    let raw = %s %S in\n" loader
          (file_contents full_name);
        Format.printf "    [|\n";
        List.iter (Format.printf "      %s raw %s;\n" extract) parts;
        Format.printf "    |]@.")

and gen_ml files cwd =
  let files =
    Array.fold_left
      (fun map sysname ->
        let basename, ext = split_file_ext sysname in
        match extension_loader ~sysname ~basename ~ext with
        | Some loader ->
            let old_data =
              Option.value (StringMap.find_opt ext map) ~default:[]
            in
            StringMap.add ext
              ((Filename.concat cwd sysname, basename, loader) :: old_data)
              map
        | None -> map)
      StringMap.empty files
  in
  StringMap.iter
    (fun ext files ->
      Format.printf "module %s = struct@." ext;
      List.iter output_file files;
      Format.printf "end@.include %s@." ext)
    files

let run () =
  let _ = Tsdl_image.init Tsdl_image.Init.(jpg + png) in
  list_files gen_ml;
  Tsdl_image.quit ()
