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

let normalize_name name =
  String.map
    (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_')
    name

let file_contents filename =
  let h = open_in_bin filename in
  let r = In_channel.input_all h in
  close_in h;
  r
