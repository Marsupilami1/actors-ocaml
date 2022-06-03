(* Adapted from the ping pong exemple *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml


module rec MessageRing : sig
  type memory = { mutable next : memory RingMember.t Option.t }
  type 'a t =
    | Send : int -> unit t
    | CreateRing : int * int * memory RingMember.t -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end
= struct
  type memory = { mutable next : memory RingMember.t Option.t }
  type 'a t =
    | Send : int -> unit t
    | CreateRing : int * int * memory RingMember.t -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end
and RingMember : sig
  type 'm t
  val create : (unit -> 'm) -> ('m t -> MessageRing.method_type) -> 'm t
  val send : 'm t -> 'a MessageRing.t -> 'a Promise.t
  val get_memory : 'm t -> 'm
  val set_memory : 'm t -> 'm -> unit
  val run : 'm t -> unit
end = Actor.Make(MessageRing)

let init = fun _ -> { MessageRing.next = None }

let rec ring_methods
  : type a . MessageRing.memory RingMember.t -> a MessageRing.t -> a =
  fun self -> function
    | Send(n) ->
      if n < 0 then () else begin
        (* Printf.printf "%d\n%!" n; *)
        let m = RingMember.get_memory self in
        match m.next with
        | None -> print_endline "abort"
        | Some next -> Promise.get @@ RingMember.send next (Send(n-1))
      end
    | CreateRing(id, size, leader) ->
      let m = RingMember.get_memory self in
      if id >= size then begin
        m.next <- Some leader;
        RingMember.set_memory self m
      end else begin
        let next = RingMember.create init actor_ring_methods in
        RingMember.run next;
        m.next <- Some next;
        RingMember.set_memory self m;
        Promise.get @@ RingMember.send next (CreateRing(id+1, size, leader))
      end
and actor_ring_methods self = {
  MessageRing.m = fun s -> ring_methods self s
}


let leader = RingMember.create init actor_ring_methods

let _ =
  print_endline "-----TEST RING------";
  RingMember.run leader;
  let p = RingMember.send leader (CreateRing(1, 100, leader)) in
  Promise.wait_and_get p;
  print_endline "Ring created";

  let p' = RingMember.send leader (Send(10_000)) in
  Promise.wait_and_get p';
  print_endline "Test passed";
  print_endline "--------------------";
