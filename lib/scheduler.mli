(* Module type for schedulers. *)
module type S = sig
  (** [Stop] is raised when the scheduler terminates. *)
  exception Stop

  (** Actors can send [Interrupt] to abort the current
      computation. *)
  exception Interrupt

  (** The type of the scheduler. *)
  type t

  (** The type of processes. *)
  type process = unit -> unit

  (** [create ()] makes a new scheduler. *)
  val create : unit -> t

  (** [push_process s p] adds the process [p] to the
      scheduler [s]. *)
  val push_process : t -> process -> unit

  (** [wait_for condition] actively waits for [condition ()]
      to be [true]. *)
  val wait_for : (unit -> bool) -> unit

  (** [yield ()] pushes the current process back to the queue. *)
  val yield : unit -> unit

  (** [run s] runs the scheduler in a new domain. *)
  val run : t -> unit Domain.t

  (** [stop s] stops the scheduler and raises {!Stop}. *)
  val stop : t -> unit
end
