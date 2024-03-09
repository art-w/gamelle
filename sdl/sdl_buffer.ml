open Common
open Ctypes

let write_never _ = assert false

let rw_ops_opt : Tsdl.Sdl.rw_ops option typ =
  Obj.magic (ptr_opt (structure "SDL_RWops"))

let some_to_ok t =
  let open Ctypes in
  let read = function Some v -> Ok v | None -> Error (`Msg "some_to_ok") in
  view ~read ~write:write_never t

let rw_from_const_mem =
  let open Ctypes in
  let open Foreign in
  foreign "SDL_RWFromConstMem"
    (ptr char @-> int @-> returning (some_to_ok rw_ops_opt))

type t = { binstring : char Ctypes.CArray.t; buffer : Tsdl.Sdl.rw_ops }

let load binstring =
  let len = String.length binstring in
  let binstring = Ctypes.CArray.of_string binstring in
  let ptr = Ctypes.CArray.start binstring in
  let& buffer = rw_from_const_mem ptr len in
  { binstring; buffer }

let get t = t.buffer
