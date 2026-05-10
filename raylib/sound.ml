open Common
open Gamelle_common

(* LoadMusicStreamFromMemory stores the data pointer for streaming, but ctypes
   frees the temporary buffer after the call. We write to a temp file instead
   so raylib reads from a persistent file path. *)

let music_temp_files : string list ref = ref []

let detect_ext binstring =
  let starts_with prefix =
    String.length binstring >= String.length prefix
    && String.sub binstring 0 (String.length prefix) = prefix
  in
  if starts_with "RIFF" then ".wav"
  else if starts_with "fLaC" then ".flac"
  else if starts_with "OggS" then ".ogg"
  else if starts_with "ID3" then ".mp3"
  else if
    String.length binstring >= 2
    && Char.code binstring.[0] = 0xFF
    && Char.code binstring.[1] land 0xE0 = 0xE0
  then ".mp3"
  else ".wav"

let load_music_from_file binstring =
  let ext = detect_ext binstring in
  let tmp = Filename.temp_file "gamelle_sound" ext in
  music_temp_files := tmp :: !music_temp_files;
  let oc = open_out_bin tmp in
  output_string oc binstring;
  close_out oc;
  Raylib.load_music_stream tmp

type sound_delayed = (io, Raylib.Sound.t) Delayed.t
type music_delayed = (io, Raylib.Music.t) Delayed.t

(* LoadWaveFromMemory is safe: it fully decodes into wave.data before returning *)
let load_sound binstring =
  Delayed.make @@ fun ~io:_ ->
  let ext = detect_ext binstring in
  let wave =
    Raylib.load_wave_from_memory ext binstring (String.length binstring)
  in
  let sound = Raylib.load_sound_from_wave wave in
  Raylib.unload_wave wave;
  sound

let load_music binstring =
  Delayed.make @@ fun ~io:_ -> load_music_from_file binstring

type data = {
  sound_delayed : sound_delayed;
  music_delayed : music_delayed;
  mutable loaded_music : Raylib.Music.t option;
}

let load str =
  {
    sound_delayed = load_sound str;
    music_delayed = load_music str;
    loaded_music = None;
  }

let ensure_music_loaded ~io data =
  match data.loaded_music with
  | Some m -> m
  | None ->
      let m = Delayed.force ~io data.music_delayed in
      data.loaded_music <- Some m;
      m

let data_duration data =
  match data.loaded_music with
  | Some m -> Raylib.get_music_time_length m
  | None -> 0.0

let play_until_end ~io data =
  let sound = Delayed.force ~io data.sound_delayed in
  Raylib.play_sound sound

let current_music : Raylib.Music.t option ref = ref None

let update_current_music () =
  Option.iter Raylib.update_music_stream !current_music

let play_music ~io (data : data) =
  let new_m = ensure_music_loaded ~io data in
  match !current_music with
  | Some m when Raylib.is_music_stream_playing m ->
      if m == new_m then ()
      else begin
        Raylib.stop_music_stream m;
        current_music := Some new_m;
        Raylib.play_music_stream new_m
      end
  | _ ->
      current_music := Some new_m;
      Raylib.play_music_stream new_m

let stop_music ~io:_ =
  Option.iter
    (fun m ->
      Raylib.stop_music_stream m;
      current_music := None)
    !current_music

let cleanup () =
  Option.iter Raylib.stop_music_stream !current_music;
  current_music := None;
  List.iter (fun f -> try Sys.remove f with _ -> ()) !music_temp_files;
  music_temp_files := []

type status = Idle | Playing | Done

type t = { music : Raylib.Music.t; mutable status : status }

let init ~io (data : data) =
  let music = ensure_music_loaded ~io data in
  { music; status = Idle }

let duration t = Raylib.get_music_time_length t.music
let current_time t = Raylib.get_music_time_played t.music
let time_left t = Float.max 0.0 (duration t -. current_time t)

let play ~io:_ t =
  match t.status with
  | Done -> false
  | Idle ->
      Raylib.play_music_stream t.music;
      t.status <- Playing;
      true
  | Playing ->
      Raylib.update_music_stream t.music;
      if Raylib.is_music_stream_playing t.music then true
      else begin
        t.status <- Done;
        false
      end

let play_loop ~io t =
  if not (play ~io t) then begin
    t.status <- Idle;
    ignore (play ~io t)
  end
