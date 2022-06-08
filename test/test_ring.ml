(* Adapted from the ping pong exemple *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml


module rec MessageRing : sig
  type memory = {
    mutable next : memory RingMember.t Option.t;
    mutable rn : RingMember.running Option.t
  }
  type 'a t =
    | Send : int -> unit t
    | Stop : memory RingMember.t -> unit t
    | CreateRing : int * int * memory RingMember.t -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
= struct
  type memory = {
    mutable next : memory RingMember.t Option.t;
    mutable rn : RingMember.running Option.t
  }
  type 'a t =
    | Send : int -> unit t
    | Stop : memory RingMember.t -> unit t
    | CreateRing : int * int * memory RingMember.t -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
and RingMember : sig
  type 'm t
  val create : (unit -> 'm) -> ('m t -> MessageRing.method_type) -> 'm t
  val send : 'm t -> 'a MessageRing.t -> 'a Promise.t
  val get_memory : 'm t -> 'm
  val set_memory : 'm t -> 'm -> unit
  type running
  val run : 'm t -> running
  val stop : 'm t -> running -> unit
end = Actor.Make(MessageRing)

let init = fun _ -> { MessageRing.next = None; MessageRing.rn = None }

let rec ring_methods
  : type a . MessageRing.memory RingMember.t
    -> (a Promise.t -> a)
    -> a MessageRing.t
    -> a
  = fun self forward -> function
    | Send(n) ->
      if n < 0 then () else begin
        (* Printf.printf "%d\n%!" n; *)
        let m = RingMember.get_memory self in
        match m.next with
        | None -> print_endline "abort"
        | Some next -> forward @@ RingMember.send next (Send(n-1))
      end
    | CreateRing(id, size, leader) ->
      let m = RingMember.get_memory self in
      if id >= size then begin
        m.next <- Some leader;
        RingMember.set_memory self m
      end else begin
        let next = RingMember.create init actor_ring_methods in
        let rn = RingMember.run next in
        m.next <- Some next;
        m.rn <- Some rn;
        RingMember.set_memory self m;
        forward @@ RingMember.send next (CreateRing(id+1, size, leader))
      end
    | Stop(leader) ->
      let m = RingMember.get_memory self in
      let next = Option.get m.next in
      if next != leader then begin
        forward @@ RingMember.send next (Stop(leader));
        RingMember.stop next (Option.get m.rn)
      end


and actor_ring_methods self = {
  MessageRing.m = fun s -> ring_methods self s
}


let leader = RingMember.create init actor_ring_methods

let _ =
  print_endline "-----TEST RING------";
  let rl = RingMember.run leader in
  let p = RingMember.send leader (CreateRing(1, 100, leader)) in
  Promise.get p;
  print_endline "Creation: Done";

  let p' = RingMember.send leader (Send(1_000_000)) in
  Promise.get p';
  print_endline "Cycle: Done";

  let p'' = RingMember.send leader (Stop(leader)) in
  Promise.get p'';
  RingMember.stop leader rl;
  print_endline "Stop: Done";

  print_endline "Test passed";
  print_endline "--------------------";
