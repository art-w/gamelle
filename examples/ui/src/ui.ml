open Gamelle
open Geometry
module Ui = Ui_

type state = unit

let () =
  Gamelle.run Box.zero @@ fun ~io box ->
  if Event.is_pressed ~io `escape then raise Exit;
  show_cursor true;
  let io = View.drawing_box box io in
  (* let io = View.clipped Box.(v_corners (o box) (mid box)) io in *)
  let box =
    snd
      Ui.(
        ui ~io ~id:0
          P2.(v 0. 0.)
          (fun ui ->
            label ~ui "This is a label";
            scroll_box ~ui ~id:4 ~size:100. (fun () ->
                if button ~ui "button" then print_endline "button pressed";
                if button ~ui "button" then print_endline "button pressed";
                if button ~ui "button" then print_endline "button pressed";
                if button ~ui "button" then print_endline "button pressed";
                if button ~ui "button" then print_endline "button pressed");
            horizontal ~ui (fun () ->
                if button ~ui "button 1" then print_endline "button 1 pressed";
                vertical ~ui (fun () ->
                    if button ~ui "button 2" then
                      print_endline "button 2 pressed";
                    if button ~ui "button 3" then
                      print_endline "button 3 pressed");
                if button ~ui "button 4" then print_endline "button 4 pressed");
            ignore @@ checkbox ~ui ~id:1 "This is a checkbox 🤓";
            let number = slider ~ui ~id:2 ~w:200. ~min:10. ~max:20. in
            label ~ui (Printf.sprintf "The slider value is %f" number)))
  in

  View.clip box ~io
  @@ draw_string ~color:Color.red Font.default ~size:29
       "CLIPCLIPCLIPCLIPCLIPCLIPCLIP" (Box.mid box);
  box
