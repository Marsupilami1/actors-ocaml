module type S = sig
  val name : string
  val untype : Untypeast.mapper
end

module Make (M : S) = struct

  let get_source_of_str = function
    | [] -> None
    | s::_ -> Some s.Parsetree.pstr_loc.loc_start.pos_fname

  let get_source_of_sg = function
    | [] -> None
    | s::_ -> Some s.Parsetree.psig_loc.loc_start.pos_fname

  let with_info ~source_file f = 
    Clflags.dont_write_files := true;
    Warnings.parse_alert_option "-24-70";
    Compile_common.with_info
      ~native:false
      ~tool_name:M.name
      ~output_prefix:"foo"
      ~dump_ext:".dump"
      ~source_file
      f

  let warn_and_error_of_error = function
    | Location.Error e ->
      Ast_mapper.extension_of_error e
    | exn ->
      match Location.error_of_exn exn with
      | Some (`Ok e) -> 
        Ast_mapper.extension_of_error e
      | Some `Already_displayed ->
        let loc = Location.in_file !Location.input_name in
        let e = Location.errorf ~loc "Unknown error" in
        Ast_mapper.extension_of_error e
      | None ->
        let loc = Location.in_file !Location.input_name in
        let e = Location.errorf ~loc "Uncaught exception: %s" (Printexc.to_string exn) in
        Ast_mapper.extension_of_error e

  let impl str =
    let source_file = match get_source_of_str str with
      | Some n -> n
      | None -> "no file info.xxx"
    in
    with_info ~source_file @@ fun info ->
    try
      let tystr = Compile_common.typecheck_impl info str in
      M.untype.structure M.untype tystr.structure
    with e ->
      [ Ast_helper.Str.extension @@ warn_and_error_of_error e ]

  let intf sg = 
    let source_file = match get_source_of_sg sg with
      | Some n -> n
      | None -> "no file info.xxx"
    in
    with_info ~source_file @@ fun info ->
    try
      let tysg = Compile_common.typecheck_intf info sg in
      M.untype.signature M.untype tysg
    with e ->
      [ Ast_helper.Sig.extension @@ warn_and_error_of_error e ]

  let register () =
    Ppxlib.Driver.register_transformation_using_ocaml_current_ast
      ~impl
      ~intf
      "name_resolver"
end
