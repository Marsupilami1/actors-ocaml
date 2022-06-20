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

let rec cat_maybes l =
  match l with
  | [] -> []
  | Some(x)::xs -> x :: cat_maybes xs
  | None::xs -> cat_maybes xs

let transform =
  object (_self)
    inherit Ast_traverse.map as super

    method! expression expr =
      let prev_default_loc = !default_loc in
      default_loc := expr.pexp_loc;
      (* let { pexp_attributes; _ } = expr in *)
      (* variant polymorphe pour les appels de méthodes *)
      let new_expr =
        match expr with
        (* object%actor ... end *)
        (* move self to function self -> obj *)
        | [%expr [%actor [%e? {pexp_desc = Pexp_object class_struct; _} as e]]] ->
          (* get all `val` fields (will be usefull for DLS) *)
          let get_val x =
            match x.pcf_desc with
            | Pcf_val (name, _, Cfk_concrete(_, e)) -> Some (name, e)
            | _ -> None
          in
          let val_fields = cat_maybes @@ List.map get_val @@ class_struct.pcstr_fields in
          (* If self is defined or not *)
          begin
            match class_struct.pcstr_self.ppat_desc with
            | Ppat_var _ -> () (* object (self) ... end *)
            | Ppat_any -> () (* object ... end *)
            | _ -> ();
          end;
          let add_DLS val_fields field =
            let new_desc = begin
              match field.pcf_desc with
              | Pcf_val _ -> None
              | Pcf_method(label, flag, Cfk_concrete(override, exp)) -> begin
                  (* call values '$actor_mem' *)
                  let f, exp =
                    match exp.pexp_desc with
                    | Pexp_poly(e, ty) ->
                      let f e = {exp with pexp_desc = Pexp_poly(e, ty)} in (f, e)
                    | _ -> (Fun.id, e)
                  in
                  let loc = exp.pexp_loc in
                  (* (a, ..., z) *)
                  let pattern = {
                    ppat_desc = Ppat_tuple (List.map (fun x -> {
                          ppat_desc = Ppat_var (fst x);
                          ppat_loc = loc;
                          ppat_loc_stack = e.pexp_loc_stack;
                          ppat_attributes = e.pexp_attributes;
                        })
                        val_fields);
                    ppat_loc = loc;
                    ppat_loc_stack = e.pexp_loc_stack;
                    ppat_attributes = e.pexp_attributes;
                  } in
                  Option.some @@
                  Pcf_method(label, flag,
                             Cfk_concrete(override,
                                          f [%expr
                                            let [%p pattern] = Domain.DLS.get _mem in
                                            [%e exp]
                                          ]))
                end
              | f -> Option.some f
            end in Option.map (fun d -> { field with pcf_desc = d }) new_desc
          in

          let loc = e.pexp_loc in
          let new_fields = {
            pcf_desc =
              Pcf_val (make_str "_mem", Immutable,
                       Cfk_concrete(
                         Fresh, [%expr
                           Domain.DLS.new_key (
                             fun _ -> [%e
                                (* (e_a, ..., e_z) *)
                               { pexp_desc = Pexp_tuple(List.map snd val_fields);
                                 pexp_loc = loc; pexp_loc_stack = e.pexp_loc_stack;
                                 pexp_attributes = e.pexp_attributes;
                               }
                             ])
                         ]))
          ; pcf_loc = loc; pcf_attributes = e.pexp_attributes
          } :: (cat_maybes @@ List.map (add_DLS val_fields) class_struct.pcstr_fields) in
          [%expr
            Oactor.create [%e {
              e with pexp_desc = Pexp_object {
                class_struct with pcstr_fields = new_fields
              }}]
          ]

        (* object#!method *)
        | [%expr [%e? obj] #! [%e? meth]] as expr ->
          let loc = expr.pexp_loc in
          let application = {
            expr with
            pexp_desc = Pexp_send([%expr [%e obj].Oactor.methods], make_str @@ exp_to_string meth)
          } in
          [%expr
            let p, fill = Promise.create () in
            Roundrobin.push_process [%e obj].Oactor.scheduler
              (fun _ -> fill [%e application]);
            p
          ]

        (* object#!method args *)
        | { pexp_desc =
              Pexp_apply (
                ([%expr [%e? obj] #! [%e? meth]] as prop),
                args
              )
          ; _
          } as r ->
          let meth_name = exp_to_string meth in
          let loc = prop.pexp_loc in
          let application = {
            r with
            pexp_desc =
              Pexp_apply (
                { prop with
                  pexp_desc =
                    Pexp_send (
                      [%expr [%e obj].methods],
                      make_str ~loc:meth.pexp_loc meth_name
                    )
                }, args
              )
          } in
          let loc = prop.pexp_loc in
          [%expr
            let (p, fill) = Promise.create () in
            Roundrobin.push_process [%e obj].Oactor.scheduler
              (fun _ -> fill [%e application]);
            p
          ]
        | _ -> super#expression expr
      in
      default_loc := prev_default_loc;
      new_expr
  end

let () =
  Driver.register_transformation
    "actor"
    ~impl:transform#structure
