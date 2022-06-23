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
  let m = 3 in
  let n = 4 in
  let r = 1000 in

  let f () = Switch.run (fun sw -> Promise.await (ackermann sw m n)) in
  let samples = Benchmark.latency1 ~name: "Ackerman(3, 4), await" (Int64.of_int r) f () in
  Benchmark.tabulate samples

let _ = Eio_main.run main
