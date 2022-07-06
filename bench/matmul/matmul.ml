open Actorsocaml

let samples = ref []
let add_samples s = samples := s @ !samples

let n = 512
let r = 40L


let random_matrix n =
  Array.init n (fun _ -> Array.init n (fun _ -> 1. -. (Random.float 2.0)))

let zero n =
  Array.make_matrix n n 0.

module With_Actor = struct
  let multiplicator = object%actor
    method compute m1 m2 res =
      let len = Array.length m1 in
      let promises = ref [] in
      for i = 0 to (len - 1) do
        let a = object%actor
          method compute =
            for j = 0 to (len - 1) do
              for k = 0 to (len - 1) do
                res.(i).(j) <- res.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
              done
            done
        end
        in promises := a#!compute :: !promises;
      done;
      List.iter Promise.await !promises
  end

  let main () =
    Random.init 42;
    let m1 = random_matrix n in
    let m2 = random_matrix n in
    let res = zero n in

    let f () = multiplicator#.compute m1 m2 res in

    let samples = Benchmark.latency1 ~name: "Actor" r f () in
    add_samples samples

  let () = Actor.Main.run main
end

module With_Domainslib = struct
  open Domainslib

  let chunk_size = 0
  let num_domains = 8

  let parallel_matrix_multiply pool a b res =
    let len = Array.length a in

    Task.parallel_for pool ~chunk_size ~start:0 ~finish:(len - 1) ~body:(fun i ->
        for j = 0 to len - 1 do
          for k = 0 to len - 1 do
            res.(i).(j) <- res.(i).(j) +. a.(i).(k) *. b.(k).(j)
          done
        done)

  let () =
    Random.init 42;
    let m1 = random_matrix n in
    let m2 = random_matrix n in
    let res = zero n in

    let f () =
      let pool = Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
      let _ = Task.run pool (fun () -> parallel_matrix_multiply pool m1 m2 res) in
      Task.teardown_pool pool
    in

    let samples = Benchmark.latency1 ~name: "Domainslib" r f () in
    add_samples samples
end

module With_Nothing = struct
  let matmul a b res =
    let len = Array.length a in
    for i = 0 to len - 1 do
        for j = 0 to len - 1 do
          for k = 0 to len - 1 do
            res.(i).(j) <- res.(i).(j) +. a.(i).(k) *. b.(k).(j)
          done
        done
      done

  let () =
    Random.init 42;
    let m1 = random_matrix n in
    let m2 = random_matrix n in
    let res = zero n in

    let f () = matmul m1 m2 res in

    let samples = Benchmark.latency1 ~name: "Nothing" r f () in
    add_samples samples
end

let process_samples samples =
  List.map (fun (s, ts) -> (s, List.map (fun t -> {t with Benchmark.utime = t.Benchmark.wall}) ts)) samples

let () =
  Benchmark.tabulate @@ process_samples !samples
