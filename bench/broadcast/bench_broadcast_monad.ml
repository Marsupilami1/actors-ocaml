open Actorsocaml
open Promise.Infix

let graph_size = 10

let actors = Array.make graph_size None

let rec forM l f =
  match l with
  | [] -> Promise.return ()
  | x::xs ->
    (f x) >>= fun px ->
    (forM xs f) >>= fun pxs ->
    Promise.return (px ; pxs)

let rec range min max =
  if min >= max then
    []
  else min :: (range (min + 1) max)

let make_actor i =
  object%actor
    val id = i
    method send n =
      if n <= 0 then Promise.return ()
      else forM (range 0 graph_size) (fun i ->
          if i <> id then
            Promise.join @@ (Option.get actors.(i))#!send (n - 1)
          else Promise.pure ()
        )
  end

let init () =
  for i=0 to graph_size - 1 do
    actors.(i) <- Some (make_actor i)
  done

let main () =
  init ();
  let r = 10 in
  let hops = 5 in
  let f () = Promise.await @@ (Option.get actors.(0))#.send hops in

  let samples = Benchmark.latency1 ~name: "Broadcast Monad" (Int64.of_int r) f () in
  Benchmark.tabulate samples


let _ =
  Actor.Main.run main
