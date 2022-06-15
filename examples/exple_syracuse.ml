open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Syracuse : int -> int Promise.t t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(Roundrobin)(MyMessage)

let actor_methods =
  let methods
    : type a . MyActor.t
      -> (a Promise.t -> a)
      -> a MyMessage.t
      -> a
    = fun self _ -> function
      | Syracuse n ->
        if n = 1 then
          Promise.pure 1
        else begin
          let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
          Promise.await @@ MyActor.send self (Syracuse(next))
          (* Promise.join @@ MyActor.send self (Syracuse(next)) *)
          (* forward @@ MyActor.send self (Syracuse(next)) *)
        end
  in fun self ->
    {MyMessage.m = fun forward -> methods self forward}


let main _ =
  let actor = MyActor.create actor_methods in

  let n = 989345275647 in
  let p = Promise.join @@ MyActor.send actor (Syracuse n) in
  assert (1 = Promise.await p)

let _ = Actor.Main.run main
