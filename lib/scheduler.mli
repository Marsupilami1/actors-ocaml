
(* Module type for schedulers. *)
module type S = sig
  (** [Stop] is raised when the scheduler terminates. *)
  exception Stop

  (** Actors can send [Interrupt] to abort the current
      computation. *)
  exception Interrupt

  (** Effect raised by [forward]. *)
  (* type _ Stdlib.Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Stdlib.Effect.t *)
  type _ Stdlib.Effect.t += Forward : (unit -> 'a) -> 'a Stdlib.Effect.t

  type pool
  
  (** The type of the scheduler. *)
  type t

  type 'a Effect.t += Spawn : (t * Domain.id) Effect.t

  (** The type of processes. *)
  type process

  val process : 'a Promise.resolver -> (unit -> 'a) -> process
  
  (** [create ()] makes a new scheduler. *)
  val create : pool -> t * Domain.id

  (** [push_process s p] adds the process [p] to the
      scheduler [s]. *)
  val push_process : t -> process -> unit

  (** [wait_for condition] actively waits for [condition ()]
      to be [true]. *)
  val wait_for : (unit -> bool) -> unit

  (** [yield ()] pushes the current process back to the queue. *)
  val yield : unit -> unit

  (** [stop s] stops the scheduler and raises {!Stop}. *)
  val stop : t -> unit
end
