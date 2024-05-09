open Gamelle

type state = unit
type k = A | B | C
type k' = k

let two_checkboxes [%ui] l1 l2 = Ui.(checkbox [%ui] l1, checkbox [%ui] l2)

let () =
  Gamelle.run Box.zero @@ fun ~io box ->
  if Input.is_pressed ~io `escape then raise Exit;
  Window.show_cursor ~io true;
  let io = View.drawing_box box io in
  let _, box =
    Ui.(
      window ~io
        Point.(v 0. 0.)
        (fun [%ui] ->
          ignore @@ text_input [%ui] 200.;
          text_area [%ui] ~width:300.
            "aaaa aaaa aaaa aaaa aaaa fffffffffffffffffffffffffffffff aaaa \
             aaaa aaaa bbb aaaa aaaa bbb aaaa aaaa bbb aaaa aaaa bbb";
          label [%ui] "This is a label ----";
          vscroll [%ui] ~height:100. (fun () ->
              if button [%ui] "button" then print_endline "button pressed";
              if button [%ui] "button" then print_endline "button pressed";
              if button [%ui] "button" then print_endline "button pressed";
              if button [%ui] "button" then print_endline "button pressed");
          horizontal [%ui] (fun () ->
              if button [%ui] "button 1" then print_endline "button 1 pressed";
              vertical [%ui] (fun () ->
                  if button [%ui] "button 2" then
                    print_endline "button 2 pressed";
                  if button [%ui] "button 3" then
                    print_endline "button 3 pressed";
                  horizontal [%ui] (fun () ->
                      if button [%ui] "button a" then
                        print_endline "button a pressed";
                      if button [%ui] "button b" then
                        print_endline "button b pressed"));
              if
                button [%ui]
                  ~style:Style.(vertical End & horizontal Center)
                  "button 4"
              then print_endline "button 4 pressed";
              if button [%ui] "button 5" then print_endline "button 5 pressed");

          if
            checkbox [%ui]
              ~style:Style.(vertical Center & horizontal Center)
              "This is a checkbox 🤓"
          then (
            ignore @@ checkbox [%ui] "Checkbox 2 !";
            if button [%ui] "for checkboxers only" then
              print_endline "YOU ARE A CHECKBOXER");
          let _number = slider [%ui] ~init:15. ~min:10. ~max:20. in
          label [%ui]
            (Printf.sprintf "The slider value is %f"
               (slider [%ui] ~min:10. ~max:20.));
          let v = radio [%ui] [ (`A, "Select A"); (`B, "Select B") ] in
          match v with
          | None -> label [%ui] "Nothing is selected"
          | Some `A -> label [%ui] "A is selected"
          | Some `B -> label [%ui] "B is selected"
          | Some `C -> label [%ui] "C is selected"))
  in
  Segment.draw ~io ~color:Color.red (Segment.v Vec.zero (Vec.v 200. 0.));
  (* draw_text ~io ~color:Color.red ~size:20 "aaaa\nbbb" Vec.zero; *)
  box
