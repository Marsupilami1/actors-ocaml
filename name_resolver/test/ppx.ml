let () =
  Printf.eprintf "Name resolver test\n%!";
  Name_resolver.M.register();
  Ppxlib.Driver.standalone ()
