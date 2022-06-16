open Actorsocaml

let x : <get : unit -> int> Oactor.t =
  object%actor
    val y = 42
    method get () = y
  end

(* Argument self should be next to the `object` key word, *)
(* but I don't know how to solve the type mismatched. *)
let ping =
  object%actor
    method ping self pong n =
      Printf.printf "Ping: %d\n%!" n;
      if n <= 0 then
        ()
      else
        Promise.await @@ pong#!pong pong self (n - 1)
  end
let pong =
  object%actor
    method pong self ping n =
      Printf.printf "Pong: %d\n%!" n;
      if n < 0 then
        ()
      else
        Promise.await @@ ping#!ping ping self (n - 1)
  end

let main _ =
  Printf.printf "%d\n" @@ Promise.await @@ x #! get ();
  Promise.await @@ ping#!ping ping pong 10


let _ = Actor.Main.run main
