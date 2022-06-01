open Actorsocaml

type message =
  | Id of int
  | ToZero of int
  | Fib of int

let methods self = function
  | Id n -> n
  | ToZero n ->
    if n <= 0 then n else begin
      Printf.printf "%d\n%!" n;
      let p = Actor.send self (ToZero (n - 1)) in
      Promise.get p
    end
  | Fib n ->
    if n < 2 then n else begin
      let p1 = Actor.send self (Fib (n - 1)) in
      let p2 = Actor.send self (Fib (n - 2)) in
      Promise.get p1 + Promise.get p2
    end

let actor = Actor.create methods

let _ =
  let _ = Id 8 in
  let p = Actor.send actor (Fib 6) in
  Actor.run actor;
  Printf.printf "%d\n%!" @@ Promise.wait_and_get p
