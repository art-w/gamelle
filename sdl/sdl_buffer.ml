open Sdl_base
open Ctypes
open Foreign

let write_never _ = assert false

type _rw_ops

let rw_ops_struct : _rw_ops structure typ = structure "SDL_RWops"
let rw_ops : _rw_ops structure ptr typ = ptr rw_ops_struct
let rw_ops_opt : Tsdl.Sdl.rw_ops option typ = Obj.magic (ptr_opt rw_ops_struct)

let rw_ops_size =
  field rw_ops_struct "size" (funptr (rw_ops @-> returning int64_t))

let rw_ops_seek =
  field rw_ops_struct "seek"
    (funptr (rw_ops @-> int64_t @-> int @-> returning int64_t))

let rw_ops_read =
  field rw_ops_struct "read"
    (funptr (rw_ops @-> ptr void @-> size_t @-> size_t @-> returning size_t))

let rw_ops_write =
  field rw_ops_struct "write"
    (funptr (rw_ops @-> ptr void @-> size_t @-> size_t @-> returning size_t))

let rw_ops_close =
  field rw_ops_struct "close" (funptr (rw_ops @-> returning int))

let _ = field rw_ops_struct "type" uint32_t

(* ... #ifdef'd union follows, we don't care we don't use Ctypes.make *)
let () = seal rw_ops_struct

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

let load ~io:_ binstring =
  let len = String.length binstring in
  let binstring = Ctypes.CArray.of_string binstring in
  let ptr = Ctypes.CArray.start binstring in
  let& buffer = rw_from_const_mem ptr len in
  (*
     Io.clean ~io (fun () ->
         (* NOT WORKING *)
         let& () = Tsdl.Sdl.rw_close buffer in
         ());
  *)
  { binstring; buffer }

let get t = t.buffer
