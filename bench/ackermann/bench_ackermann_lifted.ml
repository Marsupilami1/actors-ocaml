open Actorsocaml

let ackermann = object%actor (self)
  method compute m n =
    match m, n with
    | (0, _) -> Promise.pure @@ n + 1
    | (_, 0) -> self#!!compute (m - 1) 1
    | _ ->
      let z = Promise.join @@ self#!compute m (n - 1) in
      Promise.bind z (fun vz -> self#.compute (m - 1) vz)
end

let main _ =
  let m = 3 in
  let n = 4 in
  let r = 1000 in
  let f () = Promise.get (ackermann#!compute m n) in
  let samples = Benchmark.latency1 ~name: "Ackerman(3, 4), liftedb" (Int64.of_int r) f () in
  Benchmark.tabulate samples

let _ = Actor.Main.run main
