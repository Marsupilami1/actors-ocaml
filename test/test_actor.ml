open Actorsocaml
open Promise.Infix

type message =
  | Fib of int

let init () = Array.make 20000 None

let methods self = function
  | Fib n ->
    let m = Actor.get_memory self in
    if m.(n) <> None then
      Option.get m.(n)
    else if n < 2 then Promise.pure n else begin
      let p1 = Promise.join @@ Actor.send self (Fib (n - 1)) in
      let p2 = Promise.join @@ Actor.send self (Fib (n - 2)) in
      let pres = (+) <$> p1 <*> p2 in
      m.(n) <- Some pres;
      pres
    end

let actor = Actor.create init methods

let _ =
  print_endline "-----TEST ACTOR-----";
  Actor.run actor;
  let n = 42 in
  let p = Promise.join @@ Actor.send actor (Fib n) in
  assert (267914296 = Promise.wait_and_get p);
  print_endline "Test passed";
  print_endline "--------------------"
