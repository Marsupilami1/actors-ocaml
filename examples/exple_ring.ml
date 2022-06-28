(* Adapted from the ring example *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml

let rec ring_member id = object%actor
  val mutable next = None
  val id = id
  method create_ring id size leader =
    if id <= size then begin
      next <- Option.some @@ ring_member id;
      (Option.get next)#?create_ring (id + 1) size leader
    end else
      next <- Option.some leader

  method send hops =
    if hops > 0 then
      (Option.get next)#?send (hops-1)
    else begin
      Printf.printf "%d\n%!" id
    end
end

let main _ =
  print_endline "-----TEST RING------";
  let leader = ring_member 1 in
  leader#.create_ring 2 503 leader;
  print_endline "Creation: Done";

  leader#.send 10000;
  print_endline "Cycle: Done";

  print_endline "Test passed";
  print_endline "--------------------"


let _ = Actor.Main.run main
