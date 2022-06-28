open Actorsocaml

let cell n next = object%actor
  val state = n
  val next = next

  method sum =
    match next with
    | None -> 0
    | Some actor ->
      state + actor#?sum

  method sum_term acc =
    match next with
    | None -> acc
    | Some actor ->
      actor#?sum_term (acc + state)
end

let rec generate n =
  if n = 0 then
    cell 0 None
  else
    cell n @@ Some (generate (n - 1))

let main _ =
  let n = 10000 in
  let r = 25 in
  let nodes = generate n in
  print_endline "done.";
  let f () = nodes#.sum_term 0 in
  let samples = Benchmark.latency1 ~name: "Linked list" (Int64.of_int r) f () in
  Benchmark.tabulate samples

let _ = Actor.Main.run main
