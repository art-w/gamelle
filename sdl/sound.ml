open Common
module Delayed = Gamelle_common.Delayed
module Mixer = Tsdl_mixer.Mixer

type sound = (io, Mixer.chunk) Delayed.t

module IntSet = Set.Make (Int)

let playing = ref IntSet.empty

let load_sound binstring =
  Delayed.make @@ fun ~io ->
  let rw = Sdl_buffer.load ~io binstring in
  let& sound = Tsdl_mixer.Mixer.load_wav_rw (Sdl_buffer.get rw) 1 in
  let _ = Sys.opaque_identity rw in
  sound

let play_sound ~io sound =
  try
    let sound = Delayed.force ~io sound in
    let& channel = Mixer.play_channel (-1) sound 0 in
    Ok channel
  with Failure msg ->
    Format.printf "WARNING: play sound: %s@." msg;
    Error `No_channels_left

type music = (io, Mixer.music) Delayed.t

let load_music binstring =
  Delayed.make @@ fun ~io ->
  let rw = Sdl_buffer.load ~io binstring in
  let& music = Mixer.load_mus_rw (Sdl_buffer.get rw) 1 in
  let _ = Sys.opaque_identity rw in
  music

let play_music ~io music =
  let music = Delayed.force ~io music in
  let& _ = Tsdl_mixer.Mixer.play_music music (-1) in
  ()

type t = { sound : sound; music : music }

type playing = {
  p_sound : t;
  mutable channel : int option;
  mutable elapsed_duration : float;
  mutable unpause_time : float;
  mutable is_paused : bool;
}

let load str = { sound = load_sound str; music = load_music str }
let play ~io t = ignore @@ play_sound ~io t.sound
let current_music = ref None

let play_music ~io t =
  match !current_music with
  | Some music when t == music -> ()
  | _ ->
      current_music := Some t;
      play_music ~io t.music

let stop_music ~io:_ =
  Mixer.pause_music ();
  current_music := None

let start_playing ~io sound =
  match play_sound ~io sound.sound with
  | Error `No_channels_left -> failwith "out of channels"
  | Ok channel ->
      Mixer.pause channel;
      {
        p_sound = sound;
        channel = Some channel;
        elapsed_duration = 0.;
        unpause_time = Sys.time ();
        is_paused = true;
      }

let pause_playing p =
  match p.channel with
  | None -> failwith "Cant pause finished sound"
  | Some _ when p.is_paused -> failwith "Cant pause a sound twice"
  | Some channel ->
      p.elapsed_duration <- p.elapsed_duration +. (Sys.time () -. p.unpause_time);
      Mixer.pause channel;
      p.is_paused <- true

let continue_playing ~io:_ p =
  match p.channel with
  | None -> failwith "Cant continue playing finished sound"
  | Some channel ->
      if p.is_paused then (
        p.is_paused <- true;
        p.unpause_time <- Sys.time ());
      let is_playing = Mixer.playing (Some channel) || Mixer.paused channel in
      if is_playing then playing := IntSet.add channel !playing;
      if not is_playing then p.channel <- None;
      not is_playing

let elapsed_duration p =
  if p.is_paused then
    p.elapsed_duration
  else
    p.elapsed_duration +. (Sys.time () -. p.unpause_time)

let duration _p = failwith "FFI for Mixer.MusicDuration is to be implemented"

let sound_of_playing p = p.p_sound