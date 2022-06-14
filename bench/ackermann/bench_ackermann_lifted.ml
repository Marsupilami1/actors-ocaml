open Actorsocaml
open Promise.Infix

module AckMessage = struct
  type _ t = Calc : int * int -> int Promise.t t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module AckActor = Actor.Make(Roundrobin)(AckMessage)

let actor_methods =
  let methods
    : type a . AckActor.t
      -> (a Promise.t -> a)
      -> a AckMessage.t
      -> a
    = fun self forward -> function
      | Calc(m, n) ->
        match m, n with
        | (0, _) -> Promise.pure @@ n + 1
        | (_, 0) ->
          let result = AckActor.send self @@ Calc(m - 1, 1) in
          forward result
        | (_, _) ->
          let z = AckActor.send self @@ Calc(m, n - 1) in
          let result =
            let* vz = Promise.join z in
            AckActor.send self @@ Calc(m - 1, vz) in
          forward result
  in fun self ->
    {AckMessage.m = fun forward -> methods self forward}


let main _ =
  let a = AckActor.create actor_methods in

  let m = 3 in
  let n = 4 in
  let r = 1000 in

  let f () = Promise.get @@ Promise.join (AckActor.send a @@ AckMessage.Calc(m, n)) in
  let samples =
    Benchmark.latency1
      ~name: "Ackerman(3, 4), lifted"
      (Int64.of_int r)
      f ()
  in
  Benchmark.tabulate samples

let _ = Actor.Main.run main
