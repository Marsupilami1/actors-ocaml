open Actorsocaml
open Promise.Infix

let samples = ref []
let add_samples s = samples := s @ !samples

let r = 50L
let n = 5000

let cell n next = object%actor
  val state = n
  val next = next

  method sum =
    match next with
    | None -> 0
    | Some actor ->
      state + actor#.sum

  method sum_term acc =
    match next with
    | None -> acc
    | Some actor ->
      actor#.sum_term (acc + state)

  method sum_monad =
    match next with
    | None -> Promise.return 0
    | Some actor ->
      ((+) state) <$> Promise.join actor#!sum_monad
end

let rec generate n =
  if n = 0 then
    cell 0 None
  else
    cell n @@ Some (generate (n - 1))

module Sum = struct
  let main () =
    let nodes = generate n in
    let f () = nodes#.sum in
    let samples = Benchmark.latency1 ~name: "Basic" r f () in
    add_samples samples

  let () = Actor.Main.run main
end

module SumTerm = struct
  let main _ =
    let nodes = generate n in
    let f () = nodes#.sum_term 0 in
    let samples = Benchmark.latency1 ~name: "Tail Call" r f () in
    add_samples samples

  let _ = Actor.Main.run main
end

module SumMonad = struct
  let main () =
    let nodes = generate n in
    let f () = Promise.get @@ nodes#.sum_monad in
    let samples = Benchmark.latency1 ~name: "Monad" r f () in
    add_samples samples

  let () = Actor.Main.run main
end

let () =
  Benchmark.tabulate !samples
