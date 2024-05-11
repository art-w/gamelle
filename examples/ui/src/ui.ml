open Gamelle

type k = A | B | C

type state = {
  text : string;
  check1 : bool;
  check2 : bool;
  slider1 : float;
  slider2 : float;
  rad : k;
}

let two_checkboxes [%ui] l1 l2 = Ui.(checkbox [%ui] l1, checkbox [%ui] l2)

let initial_state =
  {
    text = "hello";
    check1 = false;
    check2 = false;
    slider1 = 0.0;
    slider2 = 15.0;
    rad = A;
  }

let () =
  Gamelle.run initial_state
  @@ fun ~io { text; check1; check2; slider1; slider2; rad } ->
  if Input.is_pressed ~io `escape then raise Exit;
  Window.show_cursor ~io true;
  let wr = ref 0.0 and hr = ref 0.0 in
  let state =
    let open Ui in
    window ~io
      ~width:(fun w ->
        wr := 1.2 *. w;
        !wr)
      ~height:(fun h ->
        hr := 1.2 *. h;
        !hr)
    @@ fun [%ui] ->
    let text = text_input [%ui] text in
    text_area [%ui]
      "aaaa aaaa aaaa aaaa aaaa fffffffffffffffffffffffffffffff aaaa aaaa aaaa \
       bbb aaaa aaaa bbb aaaa aaaa bbb aaaa aaaa bbb";
    label [%ui] "This is a label ----";
    horizontal [%ui] (fun () ->
        vscroll [%ui] (fun () ->
            text_area [%ui]
              "Another long string that should wrap, and wrap, and wrap again \
               until everything is displayed. It must be longer otherwise \
               there will be no need for the scrollbar.");
        vscroll [%ui] (fun () ->
            if button [%ui] "button A" then print_endline "button pressed";
            if button [%ui] "button AB" then print_endline "button pressed";
            if button [%ui] "button ABA" then print_endline "button pressed";
            if button [%ui] "button ABAB" then print_endline "button pressed";
            if button [%ui] "button ABAB---" then print_endline "button pressed"));
    horizontal [%ui] (fun () ->
        if button [%ui] "button 1" then print_endline "button 1 pressed";
        vertical [%ui] (fun () ->
            if button [%ui] "button 2" then print_endline "button 2 pressed";
            if button [%ui] "button 3" then print_endline "button 3 pressed";
            horizontal [%ui] (fun () ->
                if button [%ui] "button a" then print_endline "button a pressed";
                if button [%ui] "button b" then print_endline "button b pressed"));
        if button [%ui] "button 4" then print_endline "button 4 pressed";
        if button [%ui] "button 5" then print_endline "button 5 pressed");

    let check1 = checkbox [%ui] "This is a checkbox 🤓" check1 in
    let check2 =
      if check1 then (
        let check2 = checkbox [%ui] "Checkbox 2 !" check2 in
        if button [%ui] "for checkboxers only" then
          print_endline "YOU ARE A CHECKBOXER";
        check2)
      else check2
    in
    let slider1 = slider [%ui] ~min:10. ~max:20. slider1 in
    let slider2 =
      horizontal [%ui] @@ fun () ->
      let slider2 = slider [%ui] ~min:10. ~max:20. slider2 in
      label [%ui] (Printf.sprintf "The slider value is %f" slider2);
      slider2
    in
    let rad =
      horizontal [%ui] @@ fun () ->
      radios [%ui] rad [ (A, "Select A"); (B, "Select B") ]
    in
    (match rad with
    | A -> label [%ui] "A is selected"
    | B -> label [%ui] "B is selected"
    | C -> label [%ui] "C is selected");
    { text; check1; check2; slider1; slider2; rad }
  in
  Window.set_size ~io (Size.v !wr !hr);
  state
