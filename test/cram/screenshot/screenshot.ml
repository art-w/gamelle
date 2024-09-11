open Webdriver_cohttp_lwt_unix
open Infix

let test =
  let* () = goto (Printf.sprintf "file://%s/%s" (Sys.getcwd ()) Sys.argv.(1)) in
  Unix.sleepf 0.5;
  let* img = screenshot () in
  return img

let host = "http://127.0.0.1:4444"

let () =
  try
    Lwt_main.run
      (let ( let* ) = Lwt.bind in
       let* str = run ~host Capabilities.firefox_headless test in
       Lwt.return @@ print_string str)
  with Webdriver e ->
    Printf.fprintf stderr "[FAIL] Webdriver error: %s\n%!" (Error.to_string e);
    Printexc.print_backtrace stderr;
    Printf.fprintf stderr "\n%!"
