open Actorsocaml

let x =
  object%actor
    val mutable y = 0
    method set n = y <- n
    method get = y
  end

let main _ =
  x#.set 42;
  Printf.printf "%d\n" @@ Promise.await @@ x#!get


let _ = Actor.Main.run main
