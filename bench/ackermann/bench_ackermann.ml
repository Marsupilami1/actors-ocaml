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
    Benchmark.latency1 ~name: "Await" 500L f ()

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
    add_samples @@ Benchmark.latency1 ~name: "EIO" 2000L f ()

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
        self#!!compute (m - 1) (Promise.await @@ self#!compute m (n - 1))
  end

  let main _ =
    let ackermann = mk_ackermann () in
    let f () = Promise.get (ackermann#!compute m n) in
    add_samples @@ Benchmark.latency1 ~name: "Forward" 2000L f ()

  let () = Actor.Main.run main
end

module Sync = struct
  open Actorsocaml

  let mk_ackermann () = object%actor (self)
    method compute m n =
      match m, n with
      | (0, _) -> n + 1
      | (_, 0) ->
        self#.compute (m - 1) 1
      | _ ->
        self#.compute (m - 1) (self#.compute m (n - 1))
  end

  let main _ =
    let ackermann = mk_ackermann () in
    let f () = Promise.get (ackermann#!compute m n) in
    add_samples @@ Benchmark.latency1 ~name: "Sync" 10000L f ()

  let () = Actor.Main.run main
end

module Native = struct
  let rec ackermann m n =
    match m, n with
    | (0, _) -> n + 1
    | (_, 0) ->
      ackermann (m - 1) 1
    | _ ->
      let z = ackermann m (n - 1) in
      ackermann (m - 1) z

  let () =
    let f () = ackermann m n in
    add_samples @@ Benchmark.latency1 ~name: "Native" 100000L f ()
end

let process_samples samples =
  List.map (fun (s, ts) -> (s, List.map (fun t -> {t with Benchmark.utime = t.Benchmark.wall}) ts)) samples

let () = Benchmark.tabulate @@ process_samples !samples
