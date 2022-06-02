open Actorsocaml
open Promise.Infix

module MyMessage = struct
  type 'a t = Fib : int -> int Promise.t t
  type method_type = { m : 'a . 'a t -> 'a }
end

module MyActor = Actor.Make(MyMessage)

type memory = unit
let init = Fun.id

let methods
  : type a . memory MyActor.t -> a MyMessage.t -> a =
  fun self -> function
  | Fib n ->
    if n < 2 then Promise.pure n else
      let p1 = Promise.join @@ MyActor.send self (Fib (n - 1)) in
      let p2 = Promise.join @@ MyActor.send self (Fib (n - 2)) in
      (+) <$> p1 <*> p2

let actor_methods self = {
  MyMessage.m = fun s -> methods self s
}

let actor = MyActor.create init actor_methods

let _ =
  MyActor.run actor;
  let n = 30 in
  let p = Promise.join @@ MyActor.send actor (Fib n) in
  Printf.printf "fib(%d) = %d\n%!" n @@ Promise.wait_and_get p
