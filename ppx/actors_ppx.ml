(* This code is heavily inspired by:
   https://github.com/ocsigen/js_of_ocaml/blob/master/ppx/ppx_js/lib_internal/ppx_js_internal.ml *)
open Ppxlib
open Ast_helper
open Asttypes
open Parsetree

exception Syntax_error of Location.Error.t

let make_exception ~loc ~sub str = Syntax_error (Location.Error.make ~loc ~sub str)


let raise_errorf ~loc fmt =
  Printf.ksprintf (fun str -> make_exception ~loc ~sub:[] str |> raise) fmt

let mkloc txt loc = { txt; loc }

let mknoloc txt = { txt; loc = Location.none }

let make_str ?loc s =
  match loc with
  | None -> mknoloc s
  | Some loc -> mkloc s loc

let exp_to_string = function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident s; _ }; _ } -> s
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident s; _ }, None); _ }
    when String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z' -> "_" ^ s
  | { pexp_loc; _ } ->
    raise_errorf
      ~loc:pexp_loc
      "Actors methods or attributes can only be simple identifiers."

let ident_of_name ~loc name = {
  pexp_desc = Pexp_ident({txt = Lident name; loc = loc});
  pexp_loc = loc;
  pexp_attributes = [];
  pexp_loc_stack = [];
}

let dls_name val_field_name = {
  val_field_name with
  txt = Printf.sprintf "$actor_val_%s" val_field_name.txt
}

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
                          Cfk_concrete(Fresh, Exp.poly meth_field.expr None));
    pcf_loc = loc;
    pcf_attributes = [];
  }

  let add_forward e =
    let loc = e.pexp_loc in
    [%expr fun [%p Pat.var (make_str "forward")] ->
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
      [%expr fun [%p Pat.var arg] -> [%e add_args_var ~i:(i+1) ps exp]]

  let rec apply_args ?(i = 0) args_list exp =
    match args_list with
    | [] -> exp
    | _::ps ->
      let loc = exp.pexp_loc in
      let arg = {loc = loc; txt = Lident(Printf.sprintf "var_%d" i)} in
      apply_args ~i:(i+1) ps [%expr [%e exp] [%e Exp.ident arg]]

  let private_name meth_field_name = {
    meth_field_name with
    txt = Printf.sprintf "$actor_meth_%s" meth_field_name.txt
  }

  let add_self_shadow self_name e =
    let loc = e.pexp_loc in
    let self_pattern = Ast_helper.Pat.var { txt = self_name; loc = Location.none } in
    let self_ident = ident_of_name ~loc:Location.none self_name in
    [%expr let [@warning "-26"] [%p self_pattern] = Actorsocaml.Oactor.Actor [%e self_ident] in [%e e]]

  let add_val_binding (val_fields : val_desc list) exp =
    let add_one_binding exp (field : val_desc) =
      let pattern = {
        ppat_desc = Ppat_var field.name;
        ppat_loc = exp.pexp_loc;
        ppat_loc_stack = exp.pexp_loc_stack;
        ppat_attributes = exp.pexp_attributes;
      } in
      let loc = exp.pexp_loc in
      [%expr let [@warning "-26"] [%p pattern] = Domain.DLS.get
                 [%e {exp with
                      pexp_desc = Pexp_ident ({
                          txt = Lident (dls_name field.name).txt;
                          loc})}] in [%e exp]]
    in List.fold_left add_one_binding exp val_fields

  let make_private self_name val_fields meth_field = {
    meth_field with
    name = private_name meth_field.name;
    flag = Public; (* TODO: solve the "implicitly public method" issue. *)
    expr =
      add_self_shadow self_name @@
      add_forward @@
      add_args meth_field.args (add_val_binding val_fields meth_field.expr);
  }

  let make_async_call self_name meth_field =
    let loc = meth_field.name.loc in
    let self = ident_of_name ~loc:loc self_name in {
      meth_field with
      expr = add_self_shadow self_name @@ add_args_var meth_field.args [%expr
          let p, fill = Actorsocaml.Promise.create () in
          let forward p' = Actorsocaml.Promise.unify p p'; raise Actorsocaml.Multiroundrobin.Interrupt in
          Actorsocaml.Oactor.send [%e self]
            (fun _ -> fill
                [%e apply_args meth_field.args @@
                  [%expr [%e Exp.send ~loc:loc
                      ([%expr Actorsocaml.Oactor.methods [%e self]])
                      (private_name meth_field.name)] forward]]);
          p]
    }

  let meth_sync_name name =
    Loc.map ~f:(fun s -> s ^ "_sync") name


  let make_sync_call self_name meth_field =
    let loc = meth_field.name.loc in
    let self = ident_of_name ~loc:loc self_name in
    { meth_field with
      name = meth_sync_name meth_field.name;
      expr = add_self_shadow self_name @@ add_args_var meth_field.args [%expr
          if Actorsocaml.Oactor.in_same_domain [%e self] then
            let forward p' = Actorsocaml.Promise.await p' in
            [%e apply_args meth_field.args @@
              [%expr [%e Exp.send ~loc:loc
                  ([%expr Actorsocaml.Oactor.methods [%e self]])
                  (private_name meth_field.name)] forward]
            ]
          else
            Actorsocaml.Promise.await @@
            [%e apply_args meth_field.args
                [%expr [%e self] #!
                    [%e ident_of_name
                        ~loc:meth_field.name.loc meth_field.name.txt]]]
        ]
    }

  let lambda_lift self_name (val_fields : val_desc list) (meth_fields : meth_desc list) =
    List.concat_map
      (fun field -> [
           make_private    self_name val_fields field;
           make_async_call self_name field;
           make_sync_call  self_name field;
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



let add_DLS_to_val_fields ~loc (val_fields : val_desc list) =
  let wrap_one_val (field : val_desc) = {
    pcf_desc =
      Pcf_val (dls_name field.name , Immutable,
               Cfk_concrete(
                 Fresh, [%expr
                   Domain.DLS.new_key (
                     fun _ -> [%e field.expr])
                 ]));
    pcf_loc = loc;
    pcf_attributes = []
  }
  in List.map wrap_one_val val_fields


let add_val_definition (val_fields : val_desc list) exp =
  let add_one_definition exp (field : val_desc) =
    let pattern = {
      ppat_desc = Ppat_var (dls_name field.name);
      ppat_loc = exp.pexp_loc;
      ppat_loc_stack = exp.pexp_loc_stack;
      ppat_attributes = exp.pexp_attributes;
    } in
    let loc = exp.pexp_loc in
    [%expr
      let [%p pattern] = Domain.DLS.new_key (fun _ -> [%e field.expr]) in
      [%e exp]
    ]
  in List.fold_left add_one_definition exp val_fields










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

(* let self = Actor self in meth.expr *)
let transform =
  object (self)
    inherit Ast_traverse.map as super

    method! expression expr =
      let prev_default_loc = !default_loc in
      default_loc := expr.pexp_loc;
      (* let { pexp_attributes; _ } = expr in *)
      let new_expr =
        match expr with
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
              self#expression @@
              add_val_definition val_fields @@ {
                e with pexp_desc = Pexp_object {
                  pcstr_fields = new_fields;
                  pcstr_self = Ast_helper.Pat.var (make_str self_name)
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
        (* val_field <- v; ... *)
        | [%expr [%e? {pexp_desc = Pexp_setinstvar(lbl, value); _} as e]; [%e? next]] ->
          let loc = e.pexp_loc in
          let dls_ident = {
            pexp_desc = Pexp_ident {txt = Lident (dls_name lbl).txt; loc};
            pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [];
          } in
          let ident = {
            ppat_desc = Ppat_var lbl;
            ppat_loc = loc; ppat_attributes = []; ppat_loc_stack = [];
          } in
          [%expr
            Domain.DLS.set [%e dls_ident] [%e value];
            let [@warning "-26"] [%p ident] = Domain.DLS.get [%e dls_ident] in
            [%e next]
          ]
        (* val_field <- v *)
        | {pexp_desc = Pexp_setinstvar(lbl, value); _} as e ->
          let loc = e.pexp_loc in
          let dls_ident = {
            pexp_desc = Pexp_ident {txt = Lident (dls_name lbl).txt; loc};
            pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [];
          } in
          [%expr Domain.DLS.set [%e dls_ident] [%e value]]
        | _ -> super#expression expr
      in
      default_loc := prev_default_loc;
      new_expr
  end

let () =
  Driver.register_transformation
    "actor"
    ~impl:transform#structure
