type ('font, 'a) s = Value of 'a | Lazy of (io:'font Io.t -> 'a)
type ('font, 'a) t = ('font, 'a) s ref

let make fn = ref (Lazy fn)

let force ~io t =
  match !t with
  | Value v -> v
  | Lazy fn ->
      let v = fn ~io in
      t := Value v;
      v
