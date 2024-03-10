open Common
module Delayed = Gamelle_common.Delayed
module Mixer = Tsdl_mixer.Mixer

type sound = Mixer.chunk Delayed.t

let load_file filename =
  Delayed.make @@ fun ~io:_ ->
  let& sound = Tsdl_mixer.Mixer.load_wav filename in
  sound

let load binstring =
  Delayed.make @@ fun ~io:_ ->
  let rw = Sdl_buffer.load binstring in
  let& sound = Tsdl_mixer.Mixer.load_wav_rw (Sdl_buffer.get rw) 1 in
  let _ = Sys.opaque_identity rw in
  sound

let play ~io sound =
  try
    let sound = Delayed.force ~io sound in
    let& _channel = Mixer.play_channel (-1) sound 0 in
    ()
  with Failure msg -> Format.printf "WARNING: play sound: %s@." msg

type music = Mixer.music Delayed.t

let load_music filename =
  Delayed.make @@ fun ~io:_ ->
  let& music = Mixer.load_mus filename in
  music

let play_music ~io music =
  let music = Delayed.force ~io music in
  let& _ = Tsdl_mixer.Mixer.play_music music (-1) in
  ()
