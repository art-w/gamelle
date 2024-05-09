open Ppxlib
module B = Ast_builder.Default

let pat_ui ~loc = B.ppat_var ~loc (Loc.make ~loc "[%ui]")
let expr_ui ~loc = B.pexp_ident ~loc (Loc.make ~loc (Lident "[%ui]"))

let ui =
  Context_free.Rule.extension
  @@ Extension.declare "ui" Extension.Context.expression
       Ast_pattern.(pstr nil)
       (fun ~loc ~path:_ ->
         let loc_string = Format.asprintf "%a" Location.print loc in
         [%expr
           Ui.update_loc [%e expr_ui ~loc]
             [%e Ast_builder.Default.(estring ~loc loc_string)]])

let rec extract_args = function
  | [%expr fun [%p? arg] -> [%e? e]] ->
      let args, expr = extract_args e in
      (arg :: args, expr)
  | e -> ([], e)

let rec build_func args expr =
  match args with
  | arg :: args ->
      let loc = arg.ppat_loc in
      [%expr fun [%p arg] -> [%e build_func args expr]]
  | [] -> expr

let traverse_ui =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      match e with
      | [%expr fun [%ui] -> [%e? subexpr]] ->
          let args, body = extract_args subexpr in
          let loc = subexpr.pexp_loc in
          let body =
            [%expr Ui.nest_loc [%e expr_ui ~loc] (fun () -> [%e body])]
          in
          let func = build_func args body in
          let loc = e.pexp_loc in
          [%expr fun [%p pat_ui ~loc] -> [%e func]]
      | _ -> e
  end

let traverse_ui_impl = traverse_ui#structure

let _ui_pat =
  Context_free.Rule.extension
  @@ Extension.declare "ui" Extension.Context.pattern
       Ast_pattern.(pstr nil)
       (fun ~loc ~path:_ -> [%pat? ui, loc])

let () =
  Driver.register_transformation "ui" ~rules:[ ui ] ~impl:traverse_ui_impl
