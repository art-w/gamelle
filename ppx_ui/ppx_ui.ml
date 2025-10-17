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

let traverse_ui =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      match e with
      | {
       pexp_loc;
       pexp_attributes;
       pexp_loc_stack;
       pexp_desc =
         Pexp_function
           ( {
               pparam_loc;
               pparam_desc = Pparam_val (Nolabel, None, [%pat? [%ui]]);
             }
             :: args,
             typ_cons,
             body );
      } ->
          let loc =
            match body with
            | Pfunction_body body -> body.pexp_loc
            | Pfunction_cases (_, loc, _) -> loc
          in
          let func =
            [%expr
              [%e
                {
                  pexp_attributes;
                  pexp_loc_stack;
                  pexp_loc;
                  pexp_desc =
                    Pexp_function
                      ( args,
                        typ_cons,
                        Pfunction_body
                          [%expr
                            Ui.nest_loc [%e expr_ui ~loc]
                              [%e
                                {
                                  pexp_attributes = [];
                                  pexp_loc_stack;
                                  pexp_loc;
                                  pexp_desc =
                                    Pexp_function
                                      ( [
                                          {
                                            pparam_loc = loc;
                                            pparam_desc =
                                              Pparam_val
                                                (Nolabel, None, [%pat? ()]);
                                          };
                                        ],
                                        None,
                                        body );
                                }]] );
                }]]
          in
          let loc = e.pexp_loc in
          [%expr fun [%p pat_ui ~loc:pparam_loc] -> [%e func]]
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
