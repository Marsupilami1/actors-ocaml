open Actorsocaml

let ackermann = object%actor (self)
  method compute m n =
    match m, n with
    | (0, _) -> n + 1
    | (_, 0) ->
      let result = self#!compute (m - 1) 1 in
      Promise.await result
    | _ ->
      let z = self#!compute m (n - 1) in
      let vz = Promise.await z in
      let result = self#!compute (m - 1) vz in
      Promise.await result
end

let main _ =
  let m = 3 in
  let n = 4 in
  let r = 50 in
  let f () = Promise.get (ackermann#!compute m n) in
  let samples = Benchmark.latency1 ~name: "Ackerman(3, 4), await" (Int64.of_int r) f () in
  Benchmark.tabulate samples

let _ = Actor.Main.run main
