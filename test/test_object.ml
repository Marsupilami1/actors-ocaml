open Actorsocaml

let x : <get : unit -> int> Oactor.t =
  object%actor
    val y = 42
    method get () = y
  end;;

let main _ =
  Printf.printf "%d\n" @@ Promise.await @@ x #! get ()

let _ = Actor.Main.run main
