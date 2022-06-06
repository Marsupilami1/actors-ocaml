(* Adapted from the ping pong exemple *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml

type memory = unit
let init = Fun.id

module rec MessagePing : sig
  type 'a t =
    | Ping : memory Pong.t * int -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end
= struct
  type 'a t =
    | Ping : memory Pong.t * int -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end
and Ping : sig
  type 'm t
  val create : (unit -> 'm) -> ('m t -> MessagePing.method_type) -> 'm t
  val send : 'm t -> 'a MessagePing.t -> 'a Promise.t
  type running
  val run : 'm t -> running
  val stop : 'm t -> running -> unit
end = Actor.Make(MessagePing)

and MessagePong : sig
  type 'a t = Pong : memory Ping.t * int -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end = struct
  type 'a t = Pong : memory Ping.t * int -> unit t
  type method_type = { m : 'a . 'a t -> 'a }
end
and Pong : sig
  type 'm t
  val create : (unit -> 'm) -> ('m t -> MessagePong.method_type) -> 'm t
  val send : 'm t -> 'a MessagePong.t -> 'a Promise.t
  type running
  val run : 'm t -> running
  val stop : 'm t -> running -> unit
end = Actor.Make(MessagePong)


let ping_methods
  : type a . memory Ping.t -> a MessagePing.t -> a =
  fun self -> function
    | Ping(pong, n) ->
      Printf.printf "Ping: %d\n%!" n;
      if n > 0 then
        Promise.await @@ Pong.send pong (Pong(self, n - 1))
let actor_ping_methods self = {
  MessagePing.m = fun s -> ping_methods self s
}

let pong_methods
  : type a . memory Pong.t -> a MessagePong.t -> a =
  fun self -> function
    | Pong(ping, n) ->
      Printf.printf "Pong: %d\n%!" n;
      if n > 0 then
        Promise.await @@ Ping.send ping (Ping(self, n - 1))
let actor_pong_methods self = {
  MessagePong.m = fun s -> pong_methods self s
}



let _ =
  print_endline "-----TEST PINGPONG------";

  let ping = Ping.create init actor_ping_methods in
  let pong = Pong.create init actor_pong_methods in
  let rping = Ping.run ping in
  let rpong = Pong.run pong in

  let p = Ping.send ping (Ping(pong, 10)) in
  Promise.get p;

  Ping.stop ping rping;
  Pong.stop pong rpong;

  print_endline "------------------------"
