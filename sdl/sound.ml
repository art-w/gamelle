open Common
module Delayed = Gamelle_common.Delayed
module Mixer = Tsdl_mixer.Mixer

type sound_chunk = (io, Mixer.chunk) Delayed.t

let load_sound binstring =
  Delayed.make @@ fun ~io ->
  let rw = Sdl_buffer.load ~io binstring in
  let& sound = Tsdl_mixer.Mixer.load_wav_rw (Sdl_buffer.get rw) 1 in
  let _ = Sys.opaque_identity rw in
  sound

let play_sound ~io sound =
  try
    let sound = Delayed.force ~io sound in
    let& _channel = Mixer.play_channel (-1) sound 0 in
    ()
  with Failure msg -> Format.printf "WARNING: play sound: %s@." msg

type music_chunk = (io, Mixer.music) Delayed.t

let load_music binstring =
  Delayed.make @@ fun ~io ->
  let rw = Sdl_buffer.load ~io binstring in
  let& music = Mixer.load_mus_rw (Sdl_buffer.get rw) 1 in
  let _ = Sys.opaque_identity rw in
  music

let play_music_chunk ~io music =
  let music = Delayed.force ~io music in
  let& _ = Tsdl_mixer.Mixer.play_music music (-1) in
  ()

type data = { sound : sound_chunk; music : music_chunk }

let load str = { sound = load_sound str; music = load_music str }

let data_duration _data = failwith "TODO"
let play_until_end ~io:_ _data = failwith "TODO"

let current_music = ref None

let play_music ~io t =
  match !current_music with
  | Some music when t == music -> ()
  | _ ->
      current_music := Some t;
      play_music_chunk ~io t.music

let stop_music ~io:_ =
  Mixer.pause_music ();
  current_music := None

type t = unit

let init ~io:_ _data = failwith "TODO"
let play ~io:_ _t = failwith "TODO"
let play_loop ~io:_ _t = failwith "TODO"
let time_left _t = failwith "TODO"
let current_time _t = failwith "TODO"
let duration _t = failwith "TODO"
