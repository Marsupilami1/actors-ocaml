(** Makes an actor from a {!Message.S}. *)
module Make (S : Scheduler.S) (M : Message.S) : sig
  (** The type of the actor, ['m] is the type of memory. *)
  type t

  (** [create init methods] returns an actor with memory
      [init ()] and methods [methods]. *)
  val create : (t -> M.method_type) -> t

  (** [async self f] computes [f] asynchronously, storing
      the result of the compoutation in a {!Promise.t}. *)
  val async : t -> (unit -> 'a) -> 'a Promise.t

  (** [send self message] sends the message [message] to
      the actor [self]. It immediatly returns a promise
      of the result. *)
  val send : t -> 'a M.t -> 'a Promise.t

  (** [wait_for condition] produces an active wait of the
      condition inside the current actor (In which the 
      method is defined). *)
  val wait_for : (unit -> bool) -> unit

  (** [run actor] starts the scheduler of [actor] *)
  val run : t -> unit

  (** [stop actor] stops the actor [actor] *)
  val stop : t -> unit
end

(** The [Main] Actor, which runs the [main] function. *)
module Main : sig
  (** Calling [run main] is mandatory in your programs. *)
  val run : (unit -> unit) -> unit
end
