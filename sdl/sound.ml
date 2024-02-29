open Common
module Mixer = Tsdl_mixer.Mixer

type sound = Mixer.chunk Lazy.t

let load filename =
  lazy
    (let& sound = Tsdl_mixer.Mixer.load_wav filename in
     sound)

let play (lazy sound) =
  try
    let& _channel = Mixer.play_channel (-1) sound 0 in
    ()
  with Failure msg -> Format.printf "WARNING: play sound: %s@." msg

type music = Mixer.music Lazy.t

let load_music filename =
  lazy
    (let& music = Mixer.load_mus filename in
     music)

let play_music (lazy music) =
  let& _ = Tsdl_mixer.Mixer.play_music music (-1) in
  ()
