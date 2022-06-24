module type S = sig
  exception Stop
  exception Interrupt
  type _ Stdlib.Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Stdlib.Effect.t

  type t
  type process = Process : ('a Promise.resolver * (unit -> 'a)) -> process

  val create : unit -> t * Domain.id
  val push_process : t -> process -> unit
  val wait_for : (unit -> bool) -> unit
  val yield : unit -> unit
  val stop : t -> unit
end
