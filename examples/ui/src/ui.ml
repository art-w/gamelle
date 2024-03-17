open Gamelle
open Geometry

module Ui = Ui_
type state = unit

let () =
  Gamelle.run Box.zero @@ fun ~io box ->
  if Event.is_pressed ~io `escape then raise Exit;
  show_cursor true;
  let io = View.drawing_box box io in
  snd
    Ui.(
      ui ~io ~id:0
        P2.(v 0. 0.)
        (fun ui ->
          label ~ui "This is a label";
          if button ~ui "This is a button" then print_endline "button pressed";
          ignore @@ checkbox ~ui ~id:1 "This is a checkbox 🤓";
          let number = slider ~ui ~id:2 ~w:200. ~min:10. ~max:20. in
          label ~ui (Printf.sprintf "The slider value is %f" number)))
