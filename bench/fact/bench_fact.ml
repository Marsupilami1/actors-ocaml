let samples = ref []
let add_samples s = samples := s @ !samples

module Term_fut = struct
  open Actorsocaml

  let rec fact_term_fut ?(acc = 1) n =
    match n with
    | 0 -> Promise.pure acc
    | _ -> fact_term_fut (n - 1) ~acc:(n * acc)

  let main () =
    let samples =
      Benchmark.latency1
        ~name: "term fut"
        10000L
        (fun n -> Promise.await @@ fact_term_fut n)
        100_000 in
    add_samples samples

  let () = Actor.Main.run main
end


module Basic_fut = struct
  open Actorsocaml
  let rec fact_basic_fut n =
    match n with
    | 0 -> Promise.pure 1
    | _ -> Promise.pure (
        Promise.await (fact_basic_fut (n - 1)) * n)

  let main () =
    let samples =
      Benchmark.latency1
        ~name: "basic fut"
        10000L
        (fun n -> Promise.await @@ fact_basic_fut n)
        10_000 in
    add_samples samples

  let () = Actor.Main.run main
end

module Monad_fut = struct
  open Actorsocaml
  open Promise.Infix
  let rec fact_basic_futm n =
    match n with
    | 0 -> Promise.pure 1
    | _ ->
      let* v = fact_basic_futm (n - 1) in
      Promise.return (v * n)

  let main () =
    let samples =
      Benchmark.latency1
        ~name: "monad fut"
        1000L
        (fun n -> Promise.await @@ fact_basic_futm n)
        10_000 in
    add_samples samples

  let () = Actor.Main.run main
end

module Fmap_Fut = struct
  open Actorsocaml
  open Promise.Infix
  let rec fact_fmap_fut n =
    match n with
    | 0 -> Promise.pure 1
    | _ -> (( * ) n) <$> fact_fmap_fut (n - 1)

  let main () =
    let samples = Benchmark.latency1
        ~name: "fmap fut"
        1000L
        (fun n -> Promise.await @@ fact_fmap_fut n)
        10_000 in
    add_samples samples

  let () = Actor.Main.run main
end

module Basic_eio = struct
  open Actorsocaml

  let rec fact_basic_eio n =
    match n with
    | 0 -> Eio.Promise.create_resolved 1
    | _ -> Eio.Promise.create_resolved (
        Eio.Promise.await (fact_basic_eio (n - 1)) * n)

  let main _ =
    let samples =
      Benchmark.latency1
        ~name: "basic eio"
        10000L
        (fun n -> Eio.Promise.await @@ fact_basic_eio n)
        10_000 in
    add_samples samples

  let _ = Eio_main.run (fun _ -> Actor.Main.run main)
end

let () =
  Benchmark.tabulate !samples
