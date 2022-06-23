open Actorsocaml
open Promise.Infix

let rec fact_term_fut ?(acc = 1) n =
  match n with
  | 0 -> Promise.pure acc
  | _ -> fact_term_fut (n - 1) ~acc:(n * acc)

let rec fact_basic_fut n =
  match n with
  | 0 -> Promise.pure 1
  | _ -> Promise.pure (
      Promise.await (fact_basic_fut (n - 1)) * n)

let rec fact_basic_futm n =
  match n with
  | 0 -> Promise.pure 1
  | _ ->
    let* v = fact_basic_futm (n - 1) in
    Promise.return (v * n)

let rec fact_fmap_fut n =
  match n with
  | 0 -> Promise.pure 1
  | _ -> (( * ) n) <$> fact_fmap_fut (n - 1)

let rec fact_basic_eio n =
  match n with
  | 0 -> Eio.Promise.create_resolved 1
  | _ -> Eio.Promise.create_resolved (
      Eio.Promise.await (fact_basic_eio (n - 1)) * n)

let main _ =
  let r = 10000 in

  let samples =
    Benchmark.latency1
      ~name: "fact term fut"
      (Int64.of_int r)
      (fun n -> Promise.await @@ fact_term_fut n)
      100_000 in
  Benchmark.tabulate samples;

  let samples =
    Benchmark.latency1
      ~name: "fact basic fut"
      (Int64.of_int r)
      (fun n -> Promise.await @@ fact_basic_fut n)
      10_000 in
  Benchmark.tabulate samples;

  let samples =
    Benchmark.latency1
      ~name: "fact basic futm"
      (Int64.of_int r)
      (fun n -> Promise.await @@ fact_basic_futm n)
      10_000 in
  Benchmark.tabulate samples;

  let samples =
    Benchmark.latency1
      ~name: "fact fmap fut"
      (Int64.of_int r)
      (fun n -> Promise.await @@ fact_fmap_fut n)
      10_000 in
  Benchmark.tabulate samples;

  let samples =
    Benchmark.latency1
      ~name: "fact basic eio"
      (Int64.of_int r)
      (fun n -> Eio.Promise.await @@ fact_basic_eio n)
      10_000 in
  Benchmark.tabulate samples

let _ = Eio_main.run (fun _ -> Actor.Main.run main)
