open Actorsocaml

type message =
  | Fib of int

let methods self = function
  | Fib n ->
    if n < 2 then n else begin
      let p1 = Actor.send self (Fib (n - 1)) in
      let p2 = Actor.send self (Fib (n - 2)) in
      Promise.get p1 + Promise.get p2
    end

let actor = Actor.create methods

let _ =
  Actor.run actor;
  let p = Actor.send actor (Fib 6) in
  Printf.printf "fib(6) = %d\n" @@ Promise.wait_and_get p
