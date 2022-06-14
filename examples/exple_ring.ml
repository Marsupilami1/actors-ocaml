(* Adapted from the ring example *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml


module rec MessageRing : sig
  type 'a t =
    | Send : int -> unit t
    | Stop : RingMember.t -> unit t
    | CreateRing : int * int * RingMember.t -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
= struct
  type 'a t =
    | Send : int -> unit t
    | Stop : RingMember.t -> unit t
    | CreateRing : int * int * RingMember.t -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
and RingMember : sig
  type t
  val create : (t -> MessageRing.method_type) -> t
  val send : t -> 'a MessageRing.t -> 'a Promise.t
end = Actor.Make(Roundrobin)(MessageRing)

type memory = {next : RingMember.t Option.t}
let rec actor_ring_methods =
  let init = fun _ -> { next = None } in
  let mem = Domain.DLS.new_key init in
  let ring_methods
    : type a . RingMember.t
      -> (a Promise.t -> a)
      -> a MessageRing.t
      -> a
    = fun _ forward -> function
      | Send(n) ->
        if n < 0 then () else begin
          (* Printf.printf "%d\n%!" n; *)
          let m = Domain.DLS.get mem in
          match m.next with
          | None -> print_endline "abort"
          | Some next -> forward @@ RingMember.send next (Send(n-1))
        end
      | CreateRing(id, size, leader) ->
        (* let m = Domain.DLS.get mem in *)
        if id >= size then begin
          Domain.DLS.set mem {next = Some leader}
        end else begin
          let next = RingMember.create actor_ring_methods in
          (* m.next <- Some next; *)
          Domain.DLS.set mem {next = Some next};
          (* Domain.DLS.set mem m; *)
          forward @@ RingMember.send next (CreateRing(id+1, size, leader))
        end
      | Stop(leader) ->
        let m = Domain.DLS.get mem in
        let next = Option.get m.next in
        (* m.next <- None; *)
        Domain.DLS.set mem {next = None};
        if next != leader then begin
          forward @@ RingMember.send next (Stop(leader))
        end
  in fun self ->
    {MessageRing.m = fun forward -> ring_methods self forward}

let main _ =
  print_endline "-----TEST RING------";
  let _ =
    let leader = RingMember.create actor_ring_methods in
    let p = RingMember.send leader (CreateRing(1, 10, leader)) in
    Promise.await p;
    print_endline "Creation: Done";

    let p' = RingMember.send leader (Send(10)) in
    Promise.await p';
    print_endline "Cycle: Done";

    let p'' = RingMember.send leader (Stop(leader)) in
    Promise.await p'';
    (* RingMember.stop leader; *)
    print_endline "Stop: Done";
  in
  print_endline "Test passed";
  print_endline "--------------------"


let _ = Actor.Main.run main
