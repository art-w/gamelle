open Cmdliner

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

let cmd_html =
  let doc = "Release HTML game" in
  let info = Cmd.info "html" ~doc in
  let run html js = inline_js_in_html html js in
  Cmd.v info Term.(const run $ html_template $ js_script)

let cmd =
  let doc = "Gamelle" in
  let version = "0.1" in
  let info = Cmd.info "gamelle" ~version ~doc in
  Cmd.group info [ cmd_assets; cmd_html ]

let () = exit (Cmd.eval cmd)
