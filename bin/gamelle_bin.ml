open Cmdliner

let is_regular_file f = Sys.file_exists f && not (Sys.is_directory f)
let is_directory f = Sys.file_exists f && Sys.is_directory f

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

let str_game_name = Str.regexp "[mM]ygame"

let replace_name ~lowercase ~uppercase str =
  Str.global_substitute str_game_name
    (fun str ->
      match Str.matched_group 0 str with
      | "mygame" -> lowercase
      | "Mygame" -> uppercase
      | s -> failwith (Printf.sprintf "unexpected %S" s))
    str

let normalize_name name =
  String.map
    (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_')
    name

let init_directory root =
  let name = normalize_name (Filename.basename root) in
  let lowercase = String.uncapitalize_ascii name in
  let uppercase = String.capitalize_ascii name in
  let replace = replace_name ~lowercase ~uppercase in
  Sys.mkdir root 0o777;
  List.iter
    (fun template_filename ->
      let filename = replace template_filename in
      mkdir_for ~root filename;
      let contents =
        replace @@ Option.get @@ Gamelle_template.read template_filename
      in
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

type loader = Raw of string | Parts of string * string * string list

let extension_loader ~sysname ~basename ~ext =
  match (basename, ext) with
  | _, "Ttf" -> Some (Raw "Gamelle.Font.load")
  | _, ("Png" | "Jpeg" | "Jpg") ->
      let raw = "Gamelle.Bitmap.load" in
      let parts = sysname ^ ".parts" in
      (* TODO: file_exists after readdir? ... *)
      if Sys.file_exists parts then
        let parts =
          file_contents parts |> String.split_on_char '\n'
          |> List.filter (( <> ) "")
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

module Assets = struct
  let cmd_edit =
    let doc = "Edit game asset" in
    let info = Cmd.info "edit" ~doc in
    let arg = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE") in
    let run file =
      Asset_editor.do_the_thing (file ^ ".parts") (file_contents file)
    in
    Cmd.v info Term.(const run $ arg)

  let cmd_bundle =
    let doc = "Bundle game assets" in
    let info = Cmd.info "pack" ~doc in
    let run () = list_files gen_ml in
    Cmd.v info Term.(const run $ const ())

  let cmd =
    let doc = "Assets handling" in
    let info = Cmd.info "assets" ~doc in
    Cmd.group info [ cmd_bundle; cmd_edit ]
end

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
  Cmd.group info [ Assets.cmd; cmd_html; cmd_init; cmd_hot ]

let () = exit (Cmd.eval cmd)
