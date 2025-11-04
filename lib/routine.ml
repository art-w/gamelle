open Effect
open Effect.Deep

type ('i, 'o, 'a) routine =
  | Finished of 'a
  | To_be_continued of 'o * ('i, 'o, 'a) continuation
  | Start

and ('i, 'o, 'a) continuation = ('i, ('i, 'o, 'a) routine) Deep.continuation

module Make (S : sig
  type i
  type o
end) =
struct
  type i = S.i
  type o = S.o
  type 'i eff += Wait_for_next_frame : o -> i eff

  let run : type a.
      (i, o, a) routine ->
      i ->
      (next_frame:(o -> i) -> i -> a) ->
      (i, o, a) routine =
   fun sync_state input f ->
    let next_frame o = perform (Wait_for_next_frame o) in
    match sync_state with
    | Start -> begin
        try Finished (f ~next_frame input)
        with effect (Wait_for_next_frame output), k ->
          To_be_continued (output, k)
      end
    | To_be_continued (_o, k) -> continue k input
    | Finished v -> Finished v
end

let run (type i o) sync_state state f =
  let module S = Make (struct
    type nonrec i = i
    type nonrec o = o
  end) in
  S.run sync_state state f
