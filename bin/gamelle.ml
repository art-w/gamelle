open Cmdliner

let file_contents filename =
  let h = open_in filename in
  let r = In_channel.input_all h in
  close_in h;
  r

let normalize_name name =
  String.map
    (fun chr ->
      if
        (chr >= 'a' && chr <= 'z')
        || (chr >= 'A' && chr <= 'Z')
        || (chr >= '0' && chr <= '9')
      then chr
      else '_')
    name

let load_extension = function
  | ".ttf" -> Some "Gamelle.Font.load "
  | ".png" | ".jpeg" | ".jpg" -> Some "Gamelle.Bitmap.load "
  | ".txt" -> Some ""
  | _ -> None

let output_file filename =
  let name = try Filename.chop_extension filename with _ -> filename in
  let name = normalize_name name in
  let ext = try Filename.extension filename with _ -> ".txt" in
  match load_extension ext with
  | None -> ()
  | Some load_fn -> (
      match file_contents filename with
      | exception _ -> ()
      | contents -> Format.printf "let %s = %s%S@." name load_fn contents)

let list_files () =
  Format.printf "(* %S *)@." (Sys.getcwd ());
  Array.iter output_file (Sys.readdir (Sys.getcwd ()))

let cmd_assets =
  let doc = "Bundle game assets" in
  let info = Cmd.info "assets" ~doc in
  let run () = list_files () in
  Cmd.v info Term.(const run $ const ())

let cmd =
  let doc = "Gamelle" in
  let version = "0.1" in
  let info = Cmd.info "gamelle" ~version ~doc in
  Cmd.group info [ cmd_assets ]

let () = exit (Cmd.eval cmd)
