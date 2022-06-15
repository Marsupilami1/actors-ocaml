open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Sum : int t
    | SumTerm : int -> int t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(Multiroundrobin)(MyMessage)

type memory = {
  mutable state : int; mutable next : MyActor.t option
}

let actor_methods s next =
  let init : unit -> memory
    = fun _ -> {state = s; next = next} in
  let mem = Domain.DLS.new_key init in
  let methods
  : type a . MyActor.t
    -> (a Promise.t -> a)
    -> a MyMessage.t
    -> a
  = fun _ forward -> function
  | Sum -> begin
    let m = Domain.DLS.get mem in
    match m.next with
    | None -> m.state
    | Some n ->
      m.state + (Promise.await @@ MyActor.send n Sum)
    end
  | SumTerm acc -> begin
    let m = Domain.DLS.get mem in
    match m.next with
    | None -> acc + m.state
    | Some n ->
      forward @@ (MyActor.send n @@ SumTerm(acc + m.state))
    end
in fun self -> {MyMessage.m = fun forward -> methods self forward}

let rec generate n =
  if n = 0 then
    MyActor.create (actor_methods n None)
  else
    MyActor.create (actor_methods n (Some (generate (n - 1))))

let main _ =
  let n = 1000 in
  let r = 1000 in
  let nodes = generate n in
  let f () = Promise.await @@ MyActor.send nodes (SumTerm 0) in
  let samples = Benchmark.latency1 ~name: "Linked list" (Int64.of_int r) f () in
  Benchmark.tabulate samples

let _ = Actor.Main.run main
