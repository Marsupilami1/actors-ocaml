module A = Ast_helper

let to_resolve env p =
  let attrs = (Env.find_value p env).val_attributes in
  List.exists (fun attr -> attr.Parsetree.attr_name.txt = "resolve") attrs

let mk_attr ~loc s id =
  let id = Ident.unique_name id in
  let loc = { loc with Location.loc_ghost = true } in
  A.Attr.mk ~loc
    (Location.mkloc s loc)
    (PStr [A.Str.eval ~loc @@ A.Exp.constant ~loc @@ A.Const.string ~loc id])

let mapper0 = Untypeast.default_mapper


let pat (type a) mapper (typat : a Typedtree.general_pattern) =
  match typat with
  | { pat_desc = Tpat_var (id, _) ;
      pat_attributes = [{ attr_name = { txt ; _}; _}] ;
      pat_loc = loc ;
      _ }
    when String.trim txt = "resolve"
    ->
    let typat = { typat with pat_attributes = [] } in
    let pat = mapper0.pat mapper typat in
    Ast_helper.Pat.attr pat @@ mk_attr ~loc "defined" id
  | _ -> mapper0.pat mapper typat

let expr mapper (tyexp : Typedtree.expression) =
  match tyexp.exp_desc with
  | Typedtree.Texp_ident (Pident id as p,_,_) ->
    (* Format.eprintf "Resolving %a with def %a@."
     *   Ident.print id
     *   (Format.pp_print_list Format.pp_print_string)
     *   (List.map (fun x -> x.Parsetree.attr_name.txt) (Env.find_value p tyexp.exp_env).val_attributes); *)
    let v = mapper0.expr mapper tyexp in
    if to_resolve tyexp.exp_env p then
      let loc = tyexp.exp_loc in
      Ast_helper.Exp.attr v @@ mk_attr ~loc "resolved" id
    else
      v            
  | _ -> mapper0.expr mapper tyexp

let name_resolver_mapper = { mapper0 with pat; expr }

module M = Untyper.Make(struct
    let name = "resolver"
    let untype = name_resolver_mapper
  end)
let () = M.register ()
