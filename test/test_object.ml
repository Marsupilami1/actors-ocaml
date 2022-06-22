open Actorsocaml

let x =
  object%actor
    val mutable y = 0
    method set n = y <- n
    method get () = y
  end

let ping =
  object%actor (self)
    method ping pong n =
      Printf.printf "Ping: %d\n%!" n;
      if n <= 0 then ()
      else pong#.pong self (n - 1)
  end
let pong =
  object%actor (self)
    method pong ping n =
      Printf.printf "Pong: %d\n%!" n;
      if n <= 0 then ()
      else forward @@ ping#!ping self (n - 1)
  end

let main _ =
  x#.set 42;
  Printf.printf "%d\n" @@ Promise.await @@ x#!get ();
  Promise.await @@ ping#!ping pong 10


let _ = Actor.Main.run main
