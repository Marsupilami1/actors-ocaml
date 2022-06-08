(** Makes an actor from a {!Message.S}. *)
module Make (S : Message.S) : sig
  (** The type of the actor, ['m] is the type of memory. *)
  type 'm t

  (** [create init methods] returns an actor with memory
      [init ()] and methods [methods]. *)
  val create : (unit -> 'm) -> ('m t -> S.method_type) -> 'm t

  (** [async self f] computes [f] asynchronously, storing
      the result of the compoutation in a {!Promise.t}. *)
  val async : 'm t -> (unit -> 'a) -> 'a Promise.t

  (** [send self message] sends the message [message] to
      the actor [self]. It immediatly returns a promise
      of the result. *)
  val send : 'm t -> 'a S.t -> 'a Promise.t

  (** [get_memory self] returns the local memory of
      the actor [self]. *)
  val get_memory : 'm t -> 'm

  (** [set_memory self m] replace [self]'s memory by [m]. *)
  val set_memory : 'm t -> 'm -> unit

  (** [wait_for condition] produces an active wait of the
      condition inside the current actor (In which the 
      method is defined). *)
  val wait_for : (unit -> bool) -> unit

  (** type of a running actor, only useful to stop the actor. *)
  type running

  (** [run actor] starts the scheduler of [actor] and returns
      a [running] actor. *)
  val run : 'm t -> running

  (** [stop actor r] stop the actor [actor], running as [r]. *)
  val stop : 'm t -> running -> unit
end
