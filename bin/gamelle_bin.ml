open Cmdliner
open Utils

let str_game_name = Str.regexp "[mM]ygame"

let replace_name ~lowercase ~uppercase str =
  Str.global_substitute str_game_name
    (fun str ->
      match Str.matched_group 0 str with
      | "mygame" -> lowercase
      | "Mygame" -> uppercase
      | s -> failwith (Printf.sprintf "unexpected %S" s))
    str

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

let re_target = Str.regexp_string "<%GAME%>"

let game_template script =
  {|<canvas id=target tabindex=1></canvas>|}
  ^ {|<script type="text/javascript">|} ^ script ^ {|</script>|}

let inline_js_in_html html js =
  let html = file_contents html in
  let js = game_template (file_contents js) in
  let html = Str.substitute_first re_target (fun _ -> js) html in
  print_endline html

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
    Cmd.v info Term.(const Asset_load.run $ const ())

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
