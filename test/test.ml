let () =
    print_endline "Hello";
    match Promise.run Actor.run with
    | Ok(_) -> ()
    | Error(e) -> raise e
