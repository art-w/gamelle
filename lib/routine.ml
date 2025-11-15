open Effect
open Effect.Deep

type ('i, 'o, 'a) t =
  | Finished of 'a
  | Running of 'o * ('i, 'o, 'a) continuation
  | Start of (next_frame:('o -> 'i) -> 'i -> 'a)

and ('i, 'o, 'a) continuation = ('i, ('i, 'o, 'a) t) Deep.continuation

let start f = Start f

module Make (S : sig
  type i
  type o
end) =
struct
  type i = S.i
  type o = S.o
  type 'i eff += Wait_for_next_frame : o -> i eff

  let tick : type a. (i, o, a) t -> i -> (i, o, a) t =
   fun sync_state input ->
    let next_frame o = perform (Wait_for_next_frame o) in
    match sync_state with
    | Start f -> begin
        try Finished (f ~next_frame input)
        with effect Wait_for_next_frame output, k ->
          Running (output, k)
      end
    | Running (_o, k) -> continue k input
    | Finished v -> Finished v
end

let tick (type i o) sync_state state =
  let module S = Make (struct
    type nonrec i = i
    type nonrec o = o
  end) in
  S.tick sync_state state
