type 'a s = Value of 'a | Lazy of (io:Io.t -> 'a)
type 'a t = 'a s ref

let make fn = ref (Lazy fn)

let force ~io t =
  match !t with
  | Value v -> v
  | Lazy fn ->
      let v = fn ~io in
      t := Value v;
      v
