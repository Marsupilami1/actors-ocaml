(** RoundRobin is a simple Scheduler. *)
include Scheduler.S

(** Main loop of the scheduler, useful for {!Actor.Main}. *)
val loop : t -> unit
