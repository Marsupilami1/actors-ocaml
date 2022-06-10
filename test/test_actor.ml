open Actorsocaml
open Promise.Infix

module MyMessage = struct
  type 'a t =
    | Fib : int -> int Promise.t t
    | Fibli : int * int * int * int -> int Promise.t t
    | ToZero : int -> unit Promise.t t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(Roundrobin)(MyMessage)


let actor_methods =
  let init : unit -> int Promise.t Option.t Array.t
    = fun _ -> Array.make 2000 None in
  let mem = Domain.DLS.new_key init in
  let methods
  : type a . MyActor.t
    -> (a Promise.t -> a)
    -> a MyMessage.t
    -> a
  = fun self forward -> function
  | Fib n ->
    let m = Domain.DLS.get mem in
    if m.(n) <> None then
      Option.get m.(n)
    else if n < 2 then Promise.pure n else begin
      let p1 = Promise.join @@ MyActor.send self (Fib (n - 1)) in
      let p2 = Promise.join @@ MyActor.send self (Fib (n - 2)) in
      let pres = (+) <$> p1 <*> p2 in
      m.(n) <- Some pres;
      pres
    end
  | Fibli(n1, n2, k, n) ->
    if k = n then Promise.pure n1 else
      forward @@ MyActor.send self (Fibli(n2, n1 + n2, k+1, n))
  | ToZero n ->
    if n < 0 then
      Promise.pure ()
    else begin
      forward @@ MyActor.send self (ToZero (n - 1))
    end
in fun self -> {MyMessage.m = fun forward -> methods self forward}


let main _ =
  print_endline "-----TEST ACTOR-----";

  let actor = MyActor.create actor_methods in
  MyActor.run actor;

  let n = 42 in
  let p = Promise.join @@ MyActor.send actor (Fib n) in
  assert (267914296 = Promise.await p);

  (* let n' = 5_000_000 in *)
  (* let p' = Promise.join @@ MyActor.send actor (ToZero n') in *)
  (* Promise.await p'; *)

  (* let n'' = 5_000_000 in *)
  (* let p'' = Promise.join @@ MyActor.send actor (Fibli(0, 1, 0, n'')) in *)
  (* ignore @@ Promise.await p''; *)

  print_endline "Test passed";
  print_endline "--------------------"


let _ = Actor.Main.run main
