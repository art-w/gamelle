open Common
open Gamelle_common

type sound_data = (io, Raylib.Sound.t) Delayed.t

let load_sound binstring =
  Delayed.make @@ fun ~io:_ ->
  let wave =
    Raylib.load_wave_from_memory ".wav" binstring (String.length binstring)
  in
  let sound = Raylib.load_sound_from_wave wave in
  Raylib.unload_wave wave;
  sound

type music_data = (io, Raylib.Music.t) Delayed.t

let load_music binstring =
  Delayed.make @@ fun ~io:_ ->
  Raylib.load_music_stream_from_memory ".ogg" binstring
    (String.length binstring)

type t = { sound : sound_data; music : music_data }

let load str = { sound = load_sound str; music = load_music str }

let play ~io t =
  let sound = Delayed.force ~io t.sound in
  Raylib.play_sound sound

let current_music : Raylib.Music.t option ref = ref None

let update_current_music () =
  Option.iter Raylib.update_music_stream !current_music

let play_music ~io t =
  match !current_music with
  | Some m when Raylib.is_music_stream_playing m ->
      let new_m = Delayed.force ~io t.music in
      if m == new_m then ()
      else begin
        Raylib.stop_music_stream m;
        current_music := Some new_m;
        Raylib.play_music_stream new_m
      end
  | _ ->
      let m = Delayed.force ~io t.music in
      current_music := Some m;
      Raylib.play_music_stream m

let stop_music ~io:_ =
  Option.iter
    (fun m ->
      Raylib.stop_music_stream m;
      current_music := None)
    !current_music

let cleanup () =
  Option.iter Raylib.stop_music_stream !current_music;
  current_music := None
