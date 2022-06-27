(* This code is heavily inspired by:
   https://github.com/ocsigen/js_of_ocaml/blob/master/ppx/ppx_js/lib_internal/ppx_js_internal.ml *)
(* open Ppxlib *)
open Astlib.Ast_500.Asttypes
open Astlib.Ast_500.Parsetree

open Ast_mapper


let mkloc txt loc = { txt; loc }

let mknoloc txt = { txt; loc = Location.none }

let mk_pat_var name = {
  ppat_desc = Ppat_var name;
  ppat_loc = name.loc;
  ppat_loc_stack = []; ppat_attributes = []
}

let mk_send ?(loc=Location.none) e lbl =
  {pexp_desc = Pexp_send(e, lbl);
   pexp_loc = loc;
   pexp_attributes = []; pexp_loc_stack = []
  }

let make_str ?loc s =
  match loc with
  | None -> mknoloc s
  | Some loc -> mkloc s loc

let exp_to_string = function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident s; _ }; _ } -> s
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident s; _ }, None); _ }
    when String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z' -> "_" ^ s
  |  _ ->
    failwith "Actors methods or attributes can only be simple identifiers."

let ident_of_name ~loc name = {
  pexp_desc = Pexp_ident({txt = Lident name; loc = loc});
  pexp_loc = loc;
  pexp_attributes = [];
  pexp_loc_stack = [];
}

let dls_name val_field_name =
  Printf.sprintf "$actor_val_%s" val_field_name

let rec cat_maybes l =
  match l with
  | [] -> []
  | Some(x)::xs -> x :: cat_maybes xs
  | None::xs -> cat_maybes xs

let map_maybes f l =
  cat_maybes (List.map f l)

type val_desc = {
  name : string loc;
  expr : expression
}

module Method = struct
  type meth_desc = {
    name : string loc;
    expr : expression;
    args : pattern list;
    flag : private_flag;
  }

  (** [destruct f] returns [(args, body)] where [f = method args = body] *)
  let rec destruct f =
    match f.pexp_desc with
    | Pexp_fun(_, _, pattern, body) ->
      let (args, e) = destruct body in
      (pattern :: args, e)
    | _ -> ([], f)

  let field_of_desc ~loc meth_field = {
    pcf_desc = Pcf_method(meth_field.name,
                          meth_field.flag,
                          Cfk_concrete(Fresh, {
                              pexp_desc = Pexp_poly(meth_field.expr, None);
                              pexp_loc = loc;
                              pexp_attributes = []; pexp_loc_stack =[]
                            }));

    (* Exp.poly meth_field.expr None)); *)
    pcf_loc = loc;
    pcf_attributes = [];
  }

  let add_unit_arg e =
    let loc = e.pexp_loc in
    [%expr fun () ->
      [%e e]
    ]

  let add_args args_list exp =
    let loc = exp.pexp_loc in
    List.fold_right (fun p e ->
        [%expr fun [%p p] -> [%e e]]
      ) args_list exp

  let rec add_args_var ?(i = 0) args_list exp =
    match args_list with
    | [] -> exp
    | _::ps ->
      let loc = exp.pexp_loc in
      let arg = {loc = loc; txt = Printf.sprintf "var_%d" i} in
      [%expr fun [%p mk_pat_var arg] -> [%e add_args_var ~i:(i+1) ps exp]]

  let rec apply_args ?(i = 0) args_list exp =
    match args_list with
    | [] -> exp
    | _::ps ->
      let loc = exp.pexp_loc in
      let arg = {loc = loc; txt = Longident.Lident(Printf.sprintf "var_%d" i)} in
      apply_args ~i:(i+1) ps [%expr [%e exp] [%e {
          pexp_desc = (Pexp_ident arg);
          pexp_loc = arg.loc;
          pexp_attributes = []; pexp_loc_stack = []
        }]]

  let private_name meth_field_name = {
    meth_field_name with
    txt = Printf.sprintf "$actor_meth_%s" meth_field_name.txt
  }

  let add_self_shadow self_name e =
    let loc = e.pexp_loc in
    let self_pattern = mk_pat_var { txt = self_name; loc = Location.none } in
    let self_ident = ident_of_name ~loc:Location.none self_name in
    [%expr
      let [%p self_pattern] =
        Actorsocaml.Oactor.Actor [%e self_ident]
      in [%e e]]

  let make_private self_name _val_fields meth_field = {
    meth_field with
    name = private_name meth_field.name;
    flag = Public; (* TODO: solve the "implicitly public method" issue. *)
    expr =
      add_self_shadow self_name @@
      add_unit_arg @@
      add_args meth_field.args meth_field.expr;
  }

  let make_async_call self_name meth_field =
    let loc = meth_field.name.loc in
    let self = ident_of_name ~loc:loc self_name in {
      meth_field with
      expr = add_self_shadow self_name @@ add_args_var meth_field.args [%expr
          let p, fill = Actorsocaml.Promise.create () in
          Actorsocaml.Oactor.send [%e self]
            (Actorsocaml.Multiroundrobin.Process(fill, (fun _ ->
                 [%e apply_args meth_field.args @@
                   [%expr [%e mk_send ~loc:loc
                       ([%expr Actorsocaml.Oactor.methods [%e self]])
                       (private_name meth_field.name)] ()]])));
          p]
    }

  let meth_sync_name name = {
    name with
    txt = name.txt ^ "_sync"
  }

  let make_sync_call self_name meth_field =
    let loc = meth_field.name.loc in
    let self = ident_of_name ~loc:loc self_name in
    { meth_field with
      name = meth_sync_name meth_field.name;
      expr = add_self_shadow self_name @@ add_args_var meth_field.args [%expr
          if Actorsocaml.Oactor.in_same_domain [%e self] then
            [%e apply_args meth_field.args @@
              [%expr [%e mk_send ~loc:loc
                  ([%expr Actorsocaml.Oactor.methods [%e self]])
                  (private_name meth_field.name)] ()]
            ]
          else
            Actorsocaml.Promise.await @@
            [%e apply_args meth_field.args
                [%expr [%e self] #!
                    [%e ident_of_name
                        ~loc:meth_field.name.loc meth_field.name.txt]]]
        ]
    }

  let meth_forward_name name = {
    name with
    txt = name.txt ^ "_forward"
  }

  let make_forward_call self_name meth_field =
    let loc = meth_field.name.loc in
    let self = ident_of_name ~loc:loc self_name in {
      meth_field with
      name = meth_forward_name meth_field.name;
      expr = add_self_shadow self_name @@ add_args_var meth_field.args [%expr
          Effect.perform @@ Actorsocaml.Multiroundrobin.Forward
            (fun forward -> Actorsocaml.Oactor.send [%e self]
                (Actorsocaml.Multiroundrobin.Process(forward, (fun _ ->
                     [%e apply_args meth_field.args @@
                       [%expr [%e mk_send ~loc:loc
                           ([%expr Actorsocaml.Oactor.methods [%e self]])
                           (private_name meth_field.name)] ()]]))));
        ]
    }

  let lambda_lift self_name (val_fields : val_desc list) (meth_fields : meth_desc list) =
    List.concat_map
      (fun field -> [
           make_private      self_name val_fields field;
           make_async_call   self_name field;
           make_sync_call    self_name field;
           make_forward_call self_name field;
         ])
      meth_fields
end





let split_fields fields =
  let worker x (vals, meths) =
    match x.pcf_desc with
    | Pcf_val (name, _, Cfk_concrete(_, e))  ->
      ({name = name; expr = e} :: vals, meths)
    | Pcf_method(name, flag, Cfk_concrete(Fresh, {pexp_desc = Pexp_poly(f, _); _})) ->
      let args, e = Method.destruct f in
      (vals, {name; Method.flag = flag; args; expr = e} :: meths)
    | _ -> (vals, meths)
  in
  List.fold_right worker fields ([] , [])


let add_val_definition (val_fields : val_desc list) exp =
  let add_one_definition exp (field : val_desc) =
    let pattern = {
      ppat_desc = Ppat_var field.name;
      ppat_loc = exp.pexp_loc;
      ppat_loc_stack = exp.pexp_loc_stack;
      ppat_attributes = {
        attr_name = mknoloc "resolve";
        attr_payload = PStr [];
        attr_loc = Location.none;
      } :: exp.pexp_attributes;
    } in
    let loc = exp.pexp_loc in
    [%expr
      let ([%p pattern]) = [%e field.expr] in
      [%e exp]
    ]
  in List.fold_left add_one_definition exp val_fields


let resolved_name l =
  let find_resolve attr = attr.attr_name.txt = "resolved"
  in
  (* get the resolved attribute if any *)
  let resolved_attribute = List.nth_opt (List.filter find_resolve l) 0 in
  (* extrat the generated name *)
  Option.bind resolved_attribute (fun attr ->
      match attr.attr_payload with
      | PStr(({pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string(s, _, _)); _}, _); _})::[]) ->
        Some s
      | _ -> None
    )
let defined_name l =
  let find_resolve attr = attr.attr_name.txt = "defined"
  in
  (* get the resolved attribute if any *)
  let resolved_attribute = List.nth_opt (List.filter find_resolve l) 0 in
  (* extrat the generated name *)
  Option.bind resolved_attribute (fun attr ->
      match attr.attr_payload with
      | PStr(({pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string(s, _, _)); _}, _); _})::[]) ->
        Some s
      | _ -> None
    )


let scheduler_fields =
  let loc = Location.none in
  let val_field_name = "$actor_scheduler_and_domain" in
  let val_field_ident = [%expr [%e ident_of_name ~loc:loc val_field_name]] in [
    Ast_helper.Cf.val_
      (make_str val_field_name)
      Immutable
      (Cfk_concrete(Fresh, [%expr Actorsocaml.Multiroundrobin.create ()]));
    Ast_helper.Cf.method_
      (make_str "scheduler")
      Public
      (Cfk_concrete(Fresh, [%expr fst [%e val_field_ident]]));
    Ast_helper.Cf.method_
      (make_str "domain")
      Public
      (Cfk_concrete(Fresh, [%expr snd [%e val_field_ident]]));
  ]

let expr_Actor (mapper : mapper) expr = match expr with
  (* object%actor ... end *)
  | [%expr [%actor [%e? {pexp_desc = Pexp_object class_struct; _} as e]]] ->
    (* get all `val` and `method` fields *)
    let val_fields, meth_fields = split_fields class_struct.pcstr_fields in

    let self_name = begin
      match class_struct.pcstr_self.ppat_desc with
      | Ppat_var name -> name.txt (* object (self) ... end *)
      | _ -> "_self" (* object ... end *)
    end in

    let loc = e.pexp_loc in
    let new_fields =
      scheduler_fields @
      (List.map (Method.field_of_desc ~loc:loc) @@ Method.lambda_lift self_name val_fields meth_fields)
    in
    [%expr
      Actorsocaml.Oactor.Actor [%e
        mapper.expr mapper @@
        add_val_definition val_fields @@ {
          e with pexp_desc = Pexp_object {
            pcstr_fields = new_fields;
            pcstr_self = mk_pat_var (make_str self_name)
          }}]
    ]
  (* object#!method *)
  | [%expr [%e? obj] #! [%e? meth]] as expr ->
    let loc = expr.pexp_loc in
    let application = {
      expr with
      pexp_desc =
        Pexp_send([%expr Actorsocaml.Oactor.methods [%e obj]],
                  make_str @@ exp_to_string meth)
    } in
    [%expr [%e application]]
  (* object#.method *)
  | [%expr [%e? obj] #. [%e? meth]] as expr ->
    let loc = expr.pexp_loc in
    let application = {
      expr with
      pexp_desc =
        Pexp_send([%expr Actorsocaml.Oactor.methods [%e obj]],
                  Method.meth_sync_name @@ make_str @@ exp_to_string meth)
    } in
    [%expr [%e application]]
  (* obejct#!!method *)
  | [%expr [%e? obj] #!! [%e? meth]] as expr ->
    let loc = expr.pexp_loc in
    let application = {
      expr with
      pexp_desc =
        Pexp_send([%expr Actorsocaml.Oactor.methods [%e obj]],
                  Method.meth_forward_name @@ make_str @@ exp_to_string meth)
    } in
    [%expr [%e application]]
  (* val_field <- v *)
  (* transform this into [(ignore val_field)[@set v]] *)
  (* We need the ignore y to resolve the name. *)
  | {pexp_desc = Pexp_setinstvar(lbl, value); _} as e ->
    let loc = Location.none in
    { e with
      pexp_desc = [%expr
        ignore [%e ident_of_name ~loc lbl.txt]
      ].pexp_desc;
      pexp_attributes = {
        attr_name = mknoloc "set";
        attr_loc = Location.none;
        attr_payload = PStr [{
            pstr_desc = Pstr_eval (value, []);
            pstr_loc = Location.none
          }]
      } :: e.pexp_attributes;
    }
  | e -> default_mapper.expr mapper e


let expr_DLS_adder (mapper : mapper) expr =
  match expr with
  (* x[@resolved "x_270"] *)
  | {pexp_desc = Pexp_ident({loc = loc; _}); pexp_attributes = l; _} ->
    let new_name_opt = resolved_name l in
    if new_name_opt = None then default_mapper.expr mapper expr
    else (
      let new_name = dls_name @@ Option.get new_name_opt in
      [%expr Domain.DLS.get [%e {expr with
                                 pexp_desc = Pexp_ident(mkloc (Longident.Lident new_name) loc);
                                 pexp_attributes = [];
                                }]]
    )
  | [%expr let [%p? { ppat_desc = Ppat_var s;
                      ppat_attributes = l; _
                    } as p] = [%e? v] in [%e? e]
  ] ->
    let new_name_opt = defined_name l in
    if new_name_opt = None then default_mapper.expr mapper expr
    else (
      let new_name = dls_name @@ Option.get new_name_opt in
      let loc = expr.pexp_loc in
      [%expr let [%p { p with
                       ppat_desc = Ppat_var({txt = new_name; loc = s.loc});
                       ppat_attributes = [];
                     }] = Domain.DLS.new_key (fun _ -> [%e v]) in [%e mapper.expr mapper e]]
    )
  (* (ignore val_field[@resolved "new_name"])[@set v] *)
  (* transform this into DLS.set new_name v *)
  | {pexp_desc =
       Pexp_apply({
           pexp_desc = Pexp_ident
               {txt = Longident.Lident "ignore"; _} ; _},
           [Nolabel, ({pexp_attributes = l; _})]
         );
     pexp_attributes = {
       attr_name = {txt = "set"; _};
       attr_payload = PStr [
           {pstr_desc = Pstr_eval (value, []);
            _}
         ]; _} :: _;
     _ } ->
    let new_name_opt = resolved_name l in
    if new_name_opt = None then default_mapper.expr mapper expr
    else (
      let new_name = Option.get new_name_opt in
      let loc = expr.pexp_loc in
      [%expr Domain.DLS.set [%e ident_of_name ~loc (dls_name new_name)] [%e value]]
    )
  | e -> default_mapper.expr mapper e

let actor_mapper = {
  default_mapper with
  expr = expr_Actor
}

let dls_adder = {
  default_mapper with
  expr = expr_DLS_adder;
}

let () =
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast
    "actor"
    ~impl:(fun s ->
        s
        |> actor_mapper.structure actor_mapper
        |> Name_resolver.M.impl
        |> dls_adder.structure dls_adder
        (* |> (fun s -> Pprintast.structure Format.std_formatter s; s) *)
      )
