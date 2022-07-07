(* Adapted from the ring example *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)

let samples = ref []

let add_samples s = samples := s @ !samples

open Actorsocaml

module Await = struct
  let rec ring_member id = object%actor
    val mutable next = None
    val id = id
    method create_ring id size leader =
      if id <= size then begin
        next <- Option.some @@ ring_member id;
        (Option.get next)#.create_ring (id + 1) size leader
      end else
        next <- Option.some leader

    method send hops =
      if hops > 0 then (
        Promise.await @@ (Option.get next)#!send (hops-1);
      )
  end

  let main _ =
    let leader = ring_member 1 in
    leader#.create_ring 2 503 leader;

    let f () = leader#.send 10000 in
    let samples = Benchmark.latency1 ~name: "Await" 50L f () in
    add_samples samples

  let _ = Actor.Main.run main
end

module Sync = struct
  let rec ring_member id = object%actor
    val mutable next = None
    val id = id
    method create_ring id size leader =
      if id <= size then begin
        next <- Option.some @@ ring_member id;
        (Option.get next)#.create_ring (id + 1) size leader
      end else
        next <- Option.some leader

    method send hops =
      if hops > 0 then (
        (Option.get next)#?send (hops-1);
      )
  end

  let main _ =
    let leader = ring_member 1 in
    leader#.create_ring 2 503 leader;

    let f () = leader#.send 10000 in
    let samples = Benchmark.latency1 ~name: "Sync" 50L f () in
    add_samples samples

  let _ = Actor.Main.run main
end

module Forward = struct
  let rec ring_member id = object%actor
    val mutable next = None
    val id = id
    method create_ring id size leader =
      if id <= size then begin
        next <- Option.some @@ ring_member id;
        (Option.get next)#.create_ring (id + 1) size leader
      end else
        next <- Option.some leader

    method send hops =
      if hops > 0 then (
        (Option.get next)#!!send (hops-1);
      )
  end

  let main _ =
    let leader = ring_member 1 in
    leader#.create_ring 2 503 leader;

    let f () = leader#.send 10000 in
    let samples = Benchmark.latency1 ~name: "Forward" 100L f () in
    add_samples samples

  let _ = Actor.Main.run main
end


let process_samples samples =
  List.map (fun (s, ts) -> (s, List.map (fun t -> {t with Benchmark.utime = t.Benchmark.wall}) ts)) samples

let () =
  Benchmark.tabulate @@ process_samples !samples
