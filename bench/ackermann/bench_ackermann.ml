let samples = ref []
let add_samples s = samples := s @ !samples

let m = 3
let n = 4

module Await = struct
  open Actorsocaml

  let mk_ackermann () = object%actor (self)
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

  let main () =
    let ackermann = mk_ackermann () in
    let f () = Promise.get (ackermann#!compute m n) in
    add_samples @@
    Benchmark.latency1 ~name: "Ackerman(3, 4), await" 50L f ()

  let () = Actor.Main.run main
end

module Eio = struct
  open Eio

  let rec ackermann sw m n =
    Fiber.fork_promise ~sw (fun _ ->
        match m, n with
        | (0, _) -> n + 1
        | (_, 0) -> Promise.await_exn @@ ackermann sw (m - 1) 1
        | (_, _) ->
          let vz = Promise.await_exn @@ ackermann sw m (n - 1) in
          let result = ackermann sw (m - 1) vz in
          Promise.await_exn result
      )

  let main _ =
    let f () = Switch.run (fun sw -> Promise.await (ackermann sw m n)) in
    add_samples @@ Benchmark.latency1 ~name: "Ackerman(3, 4), eio" 1000L f ()

  let () = Eio_main.run main
end

module Forward = struct
  open Actorsocaml

  let mk_ackermann () = object%actor (self)
    method compute m n =
      match m, n with
      | (0, _) -> n + 1
      | (_, 0) ->
        self#!!compute (m - 1) 1
      | _ ->
        let vz = self#.compute m (n - 1) in
        self#!!compute (m - 1) vz
  end

  let main _ =
    let ackermann = mk_ackermann () in
    let f () = Promise.get (ackermann#!compute m n) in
    add_samples @@ Benchmark.latency1 ~name: "Ackerman(3, 4), forward" 10000L f ()

  let () = Actor.Main.run main
end

let () =
  Benchmark.tabulate !samples
