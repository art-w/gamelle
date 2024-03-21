open Gamelle
open Geometry
module Ui = Ui_

type state = unit

let () =
  Gamelle.run Box.zero @@ fun ~io box ->
  if Event.is_pressed ~io `escape then raise Exit;
  show_cursor true;
  let io = View.drawing_box box io in
  let box =
    snd
      Ui.(
        ui ~io ~id:0
          P2.(v 0. 0.)
          (fun ui ->
            label ~ui ~id:5 "This is a label ---";
            scroll_box ~ui ~id:4
              {
                height = 100.;
                f =
                  (fun () ->
                    if button ~ui ~id:6 "button" then
                      print_endline "button pressed";
                    if button ~ui ~id:7 "button" then
                      print_endline "button pressed";
                    if button ~ui ~id:8 "button" then
                      print_endline "button pressed";
                    if button ~ui ~id:9 "button" then
                      print_endline "button pressed";
                    if button ~ui ~id:10 "button" then
                      print_endline "button pressed");
              };
            horizontal ~ui ~id:18 (fun () ->
                if button ~ui ~id:11 "button 1" then
                  print_endline "button 1 pressed";
                vertical ~ui ~id:17 (fun () ->
                    if button ~ui ~id:12 "button 2" then
                      print_endline "button 2 pressed";
                    if button ~ui ~id:13 "button 3" then
                      print_endline "button 3 pressed");
                if button ~ui ~id:14 "button 4" then
                  print_endline "button 4 pressed");
            if checkbox ~ui ~id:1 "This is a checkbox 🤓" then
              ignore @@ button ~ui ~id:16 "for checkboxers only";
            let number = slider ~ui ~id:2 { w = 200.; min = 10.; max = 20. } in
            label ~ui ~id:15 (Printf.sprintf "The slider value is %f" number)))
  in
  box
