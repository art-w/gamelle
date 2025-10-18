open Gamelle

type state = (unit, unit, string) Sync.routine

let () =
  Gamelle.run Sync.Start @@ fun ~io (state : state) ->
  match state with
  | Finished str ->
      print_endline str;
      raise Exit
  | state ->
      Sync.run state () @@ fun ~yield () ->
      let rec loop () =
        let open Ui in
        let (str, b), box =
          window ~io Point.zero @@ fun [%ui] ->
          let str = text_input [%ui] 1000. in
          let b = button [%ui] "confirm" in
          (str, b)
        in
        Window.set_size ~io (Box.size box);
        if b then str
        else
          let () = yield () in
          loop ()
      in
      loop ()
