open Effect
open Effect.Deep

type ('i, 'o, 'a) t =
  | Finished of 'a
  | Running of 'o * ('i, 'o, 'a) continuation

and ('i, 'o, 'a) continuation = ('i, ('i, 'o, 'a) t) Deep.continuation

module Make (S : sig
  type i
  type o
end) =
struct
  type i = S.i
  type o = S.o
  type 'i eff += Wait_for_next_tick : o -> i eff

  let next_frame o = perform (Wait_for_next_tick o)

  let tick : type a. (i, o, a) t -> i -> (i, o, a) t =
   fun sync_state input ->
    match sync_state with
    | Running (_o, k) -> continue k input
    | Finished v -> Finished v

  let start : type a. (next_frame:(o -> i) -> a) -> (i, o, a) t =
   fun f ->
    try Finished (f ~next_frame)
    with effect Wait_for_next_tick output, k -> Running (output, k)
end

let tick (type i o) sync_state state =
  let module S = Make (struct
    type nonrec i = i
    type nonrec o = o
  end) in
  S.tick sync_state state

let start (type i o) f =
  let module S = Make (struct
    type nonrec i = i
    type nonrec o = o
  end) in
  S.start f
