(* Adapted from the ping pong example *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml

let ping =
  object%actor (self)
    method ping pong n =
      Printf.printf "Ping: %d\n%!" n;
      if n <= 0 then ()
      else Actor.forward (pong#!pong self (n - 1))
  end

let pong =
  object%actor (self)
    method pong ping n =
      Printf.printf "Pong: %d\n%!" n;
      if n <= 0 then ()
      else ping#!!ping self (n - 1)
  end

let main _ =
  Promise.await @@ ping#!ping pong 10


let _ = Actor.Main.run main
