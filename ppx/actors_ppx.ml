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
        | [%expr [%actor [%e? {pexp_desc = Pexp_object _class_construct; _} as e]]] ->
          let loc = e.pexp_loc in
          [%expr
            let a = Oactor.create [%e self#expression e] in a
          ]
        (* object#!method args *)
        | { pexp_desc =
              Pexp_apply (
                ([%expr [%e? obj] #! [%e? meth]] as prop),
                _args
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
                }, _args
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
