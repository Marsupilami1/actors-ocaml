let () =
  Name_resolver.M.register();
  Ppxlib.Driver.standalone ()
