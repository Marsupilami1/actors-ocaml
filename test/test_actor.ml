open Actorsocaml
open Promise.Infix

module MyMessage = struct
  type 'a t =
    | Fib : int -> int Promise.t t
  type method_type = { m : 'a . 'a t -> 'a }
end

type memory = int Promise.t Option.t Array.t
let init () = Array.make 20000 None

module MyActor = Actor.Make(MyMessage)

let methods : type a . memory MyActor.t -> a MyMessage.t -> a = fun self -> function
  | Fib n ->
    let m = MyActor.get_memory self in
    if m.(n) <> None then
      Option.get m.(n)
    else if n < 2 then Promise.pure n else begin
      let p1 = Promise.join @@ MyActor.send self (Fib (n - 1)) in
      let p2 = Promise.join @@ MyActor.send self (Fib (n - 2)) in
      let pres = (+) <$> p1 <*> p2 in
      m.(n) <- Some pres;
      pres
    end


let actor_methods self = {
  MyMessage.m = fun s -> methods self s
}

let actor = MyActor.create init actor_methods

let _ =
  print_endline "-----TEST ACTOR-----";
  MyActor.run actor;
  let n = 42 in
  let p = Promise.join @@ MyActor.send actor (Fib n) in
  assert (267914296 = Promise.wait_and_get p);
  print_endline "Test passed";
  print_endline "--------------------"
