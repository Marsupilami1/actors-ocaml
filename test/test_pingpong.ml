(* Adapted from the ping pong example *)
(* https://stw.gitbooks.io/the-encore-programming-language/content/nutshell.html *)
open Actorsocaml

module rec MessagePing : sig
  type 'a t =
    | Ping : Pong.t * int -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
= struct
  type 'a t =
    | Ping : Pong.t * int -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
and Ping : sig
  type t
  val create : (t -> MessagePing.method_type) -> t
  val send : t -> 'a MessagePing.t -> 'a Promise.t
end = Actor.Make(Roundrobin)(MessagePing)

and MessagePong : sig
  type 'a t = Pong : Ping.t * int -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end = struct
  type 'a t = Pong : Ping.t * int -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
and Pong : sig
  type t
  val create : (t -> MessagePong.method_type) -> t
  val send : t -> 'a MessagePong.t -> 'a Promise.t
end = Actor.Make(Roundrobin)(MessagePong)


let actor_ping_methods =
  let ping_methods
    : type a . Ping.t
      -> (a Promise.t -> a)
      -> a MessagePing.t
      -> a
    = fun self forward -> function
      | Ping(pong, n) ->
        Printf.printf "Ping: %d\n%!" n;
        if n > 0 then
          forward @@ Pong.send pong (Pong(self, n - 1))
  in fun self ->
    {MessagePing.m = fun forward -> ping_methods self forward}

let actor_pong_methods =
  let pong_methods
    : type a . Pong.t
      -> (a Promise.t -> a)
      -> a MessagePong.t
      -> a
    = fun self forward -> function
      | Pong(ping, n) ->
        Printf.printf "Pong: %d\n%!" n;
        if n > 0 then
          forward @@ Ping.send ping (Ping(self, n - 1))
  in fun self ->
    {MessagePong.m = fun forward -> pong_methods self forward}



let main _ =
  print_endline "-----TEST PINGPONG------";

  let ping = Ping.create actor_ping_methods in
  let pong = Pong.create actor_pong_methods in

  (* TODO: solve deadlock issue *)
  let n = 10 in
  let p = Ping.send ping (Ping(pong, n)) in
  Promise.await p;

  print_endline "------------------------"

let _ = Actor.Main.run main
