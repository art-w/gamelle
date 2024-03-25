open Ppxlib

let ui =
  Extension.declare "ui" Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ ->
      let loc_string = Format.asprintf "%a" Location.print loc in
      [%expr ui, [%e Ast_builder.Default.(estring ~loc loc_string)]])

let () = Driver.register_transformation "ui" ~extensions:[ ui ]
