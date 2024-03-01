open Cmdliner

let mkdir_for ~root name =
  let parts = String.split_on_char '/' name in
  let rec go cwd = function
    | [] | [ _ ] -> ()
    | d :: ds ->
        let cwd = Filename.concat cwd d in
        if not (Sys.file_exists cwd) then Sys.mkdir cwd 0o777;
        go cwd ds
  in
  go root parts

let init_directory root =
  Sys.mkdir root 0o777;
  List.iter
    (fun filename ->
      mkdir_for ~root filename;
      let contents = Option.get @@ Gamelle_template.read filename in
      let h = open_out (Filename.concat root filename) in
      output_string h contents;
      close_out h)
    Gamelle_template.file_list

let file_contents filename =
  let h = open_in_bin filename in
  let r = In_channel.input_all h in
  close_in h;
  r

let re_target = Str.regexp_string "<%GAME%>"

let game_template script =
  {|<canvas id=target tabindex=1></canvas>|}
  ^ {|<script type="text/javascript">|} ^ script ^ {|</script>|}

let inline_js_in_html html js =
  let html = file_contents html in
  let js = game_template (file_contents js) in
  let html = Str.substitute_first re_target (fun _ -> js) html in
  print_endline html

let normalize_name name =
  String.map
    (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_')
    name

let extension_loader ~basename ~ext =
  match (basename, ext) with
  | _, "Ttf" -> Some "Gamelle.Font.load"
  | _, ("Png" | "Jpeg" | "Jpg") -> Some "Gamelle.Bitmap.load"
  | _, "Mp3" -> Some "Gamelle.Sound.load"
  | "assets", _ | "dune", "No_ext" -> None
  | _ -> Some "Fun.id"

let output_file (full_name, basename, loader) =
  if Sys.file_exists full_name then (
    Format.printf "@.  (** Generated from %s *)@." basename;
    Format.printf "  let %s = %s %S@." basename loader (file_contents full_name))

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

let gen_ml files cwd =
  let files =
    Array.fold_left
      (fun map sysname ->
        let basename, ext = split_file_ext sysname in
        match extension_loader ~basename ~ext with
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

let list_files k =
  let cwd = Sys.getcwd () in
  Format.printf "(* %S *)@." cwd;
  k (Sys.readdir cwd) cwd

let cmd_assets =
  let doc = "Bundle game assets" in
  let info = Cmd.info "assets" ~doc in
  let run () = list_files gen_ml in
  Cmd.v info Term.(const run $ const ())

let html_template =
  let env =
    let doc = "Template HTML" in
    Cmd.Env.info "" ~doc
  in
  Arg.(
    required & opt (some file) None & info [ "template" ] ~docv:"TEMPLATE" ~env)

let js_script =
  let env =
    let doc = "Js script" in
    Cmd.Env.info "" ~doc
  in
  Arg.(required & opt (some file) None & info [ "script" ] ~docv:"SCRIPT" ~env)

let game_name =
  let env =
    let doc = "Name of directory" in
    Cmd.Env.info "NAME" ~doc
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~env)

let game_file =
  let env =
    let doc = "Name of game .cmxs file" in
    Cmd.Env.info "NAME" ~doc
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~env)

let cmd_html =
  let doc = "Release HTML game" in
  let info = Cmd.info "html" ~doc in
  let run html js = inline_js_in_html html js in
  Cmd.v info Term.(const run $ html_template $ js_script)

let cmd_init =
  let doc = "Initialize a new game directory" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const init_directory $ game_name)

let cmd_hot =
  let doc = "Run game with hot reload" in
  let info = Cmd.info "hotreload" ~doc in
  Cmd.v info Term.(const Hotreload.run $ game_file)

let cmd =
  let doc = "Gamelle" in
  let version = "0.1" in
  let info = Cmd.info "gamelle" ~version ~doc in
  Cmd.group info [ cmd_assets; cmd_html; cmd_init; cmd_hot ]

let () = exit (Cmd.eval cmd)
