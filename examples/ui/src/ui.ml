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
        ui ~io
          P2.(v 0. 0.)
          (fun ui ->
            label ~ui "This is a label ---";
            scroll_box ~ui
              {
                height = 100.;
                f =
                  (fun () ->
                    if button ~ui "button" then print_endline "button pressed";
                    if button ~ui "button" then print_endline "button pressed";
                    if button ~ui "button" then print_endline "button pressed";
                    if button ~ui "button" then print_endline "button pressed";
                    if button ~ui "button" then print_endline "button pressed");
              };
            horizontal ~ui (fun () ->
                if button ~ui "button 1" then print_endline "button 1 pressed";
                vertical ~ui (fun () ->
                    if button ~ui "button 2" then
                      print_endline "button 2 pressed";
                    if button ~ui "button 3" then
                      print_endline "button 3 pressed");
                if button ~ui "button 4" then print_endline "button 4 pressed");
            if checkbox ~ui "This is a checkbox 🤓" then (
              ignore @@ checkbox ~ui "Checkbox 2 !";
              if button ~ui "for checkboxers only" then
                print_endline "YOU ARE A CHECKBOXER");
            let number = slider ~ui { w = 200.; min = 10.; max = 20. } in
            label ~ui (Printf.sprintf "The slider value is %f" number)))
  in
  box
