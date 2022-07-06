open Actorsocaml

let samples = ref []
let add_samples s =  samples := s @ !samples

let n = 989345275647

module Await = struct
let actor = object%actor (self)
  method syracuse n =
    if n = 1 then 1
    else begin
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      Promise.await @@ self#!syracuse next
    end
end

let main () =
  let f () = Promise.get @@ actor#!syracuse n in
  add_samples @@ Benchmark.latency1 ~name: "Await" 2000L f ()

let _ = Actor.Main.run main
end

module Eio = struct
  open Eio

  let actor = object (self)
    method syracuse sw n =
      Fiber.fork_promise ~sw (fun _ ->
          if n = 1 then 1
          else begin
            let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
            Promise.await_exn @@ self#syracuse sw next
          end
        )
  end

  let main _ =
    let f () = Switch.run (fun sw -> Promise.await_exn @@ actor#syracuse sw n) in
    add_samples @@ Benchmark.latency1 ~name: "Eio" 5000L f ()

  let _ = Eio_main.run main
end

module Forward = struct
  let actor = object%actor (self)
    method syracuse n =
      if n = 1 then 1
      else begin
        let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
        self#!!syracuse next
      end
  end

  let main () =
    let f () = Promise.get @@ actor#!syracuse n in
    add_samples @@ Benchmark.latency1 ~name: "Forward" 10000L f ()

  let _ = Actor.Main.run main
end

module Synchronous = struct
  let actor = object%actor (self)
    method syracuse n =
      if n = 1 then 1
      else begin
        let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
        self#.syracuse next
      end
  end

  let main () =
    let f () = Promise.get @@ actor#!syracuse n in
    add_samples @@ Benchmark.latency1 ~name: "Sync" 100000L f ()

  let _ = Actor.Main.run main
end

module Native = struct
  let actor = object (self)
    method syracuse n =
      if n = 1 then 1
      else begin
        let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
        self#syracuse next
      end
  end

  let main () =
    let f () = actor#syracuse n in
    add_samples @@ Benchmark.latency1 ~name: "Native" 1000000L f ()

  let _ = Actor.Main.run main
end

let process_samples samples =
  List.map (fun (s, ts) -> (s, List.map (fun t -> {t with Benchmark.utime = t.Benchmark.wall}) ts)) samples

let _ =
  Benchmark.tabulate @@ process_samples !samples
