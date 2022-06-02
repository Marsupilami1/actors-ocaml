open Actorsocaml

type message =
  | Fib of int

let memory = Array.make 20000 None

let methods self = function
  | Fib n ->
    let m = Actor.memory self in
    if m.(n) <> None then
      Option.get m.(n)
    else if n < 2 then n else begin
      let p1 = Actor.send self (Fib (n - 1)) in
      let v1 = Promise.get p1 in
      let p2 = Actor.send self (Fib (n - 2)) in
      let v2 = Promise.get p2 in
      let res = v1 + v2 in
      m.(n) <- Some res; res
    end

let actor = Actor.create memory methods

let _ =
  print_endline "-----TEST ACTOR-----";
  Actor.run actor;
  let n = 42 in
  let p = Actor.send actor (Fib n) in
  assert (267914296 = Promise.wait_and_get p);
  print_endline "Test passed";
  print_endline "--------------------"
