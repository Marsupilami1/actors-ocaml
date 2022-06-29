open Actorsocaml

let graph_size = 10

let actors = Array.make graph_size None

let make_actor i =
  object%actor
    val id = i
    method send n =
      if n <= 0 then ()
      else
        for i=0 to graph_size - 1 do
          if i <> id then
            Promise.await @@ (Option.get actors.(i))#!send (n - 1)
        done
  end

let init () =
  for i=0 to graph_size - 1 do
    actors.(i) <- Some (make_actor i)
  done

let main () =
  init ();
  let r = 10 in
  let f () = (Option.get actors.(0))#.send 5 in

  let samples = Benchmark.latency1 ~name: "Broadcast Basic" (Int64.of_int r) f () in
  Benchmark.tabulate samples


let _ =
  Actor.Main.run main
