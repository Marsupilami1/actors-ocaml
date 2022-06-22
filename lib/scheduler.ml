module type S = sig
  exception Stop
  exception Interrupt

  type t
  type process = unit -> unit

  val create : unit -> t * Domain.id
  val push_process : t -> process -> unit
  val wait_for : (unit -> bool) -> unit
  val yield : unit -> unit
  val stop : t -> unit
end
