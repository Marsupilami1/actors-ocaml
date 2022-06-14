(** Makes an actor from a {!Message.S}. *)
module Make (S : Scheduler.S) (M : Message.S) : sig
  (** The type of the actor. *)
  type t

  (** [create methods] returns an actor with methods [methods]. *)
  val create : (t -> M.method_type) -> t

  (** [send self message] sends the message [message] to
      the actor [self]. It immediatly returns a promise
      of the result. *)
  val send : t -> 'a M.t -> 'a Promise.t

  (** [wait_for condition] produces an active wait of the
      condition inside the current actor (In which the 
      method is defined). *)
  val wait_for : (unit -> bool) -> unit

  (** [yield ()] pushes the current process back to the queue. *)
  val yield : unit -> unit
end

(** The [Main] Actor, which runs the [main] function. *)
module Main : sig
  (** Calling [run main] is mandatory in your programs. *)
  val run : (unit -> unit) -> unit
end
