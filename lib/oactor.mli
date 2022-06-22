(** The type of the actor. *)
type 'a t

(** [create methods] returns an actor with methods [methods]. *)
val create : ('a t -> 'a) -> 'a t

(** [send actor process] tells [actor] to execute [process ()]. *)
val send : 'a t -> (unit -> unit) -> unit

(** [methods actor] returns the methods of [actor]. *)
val methods : 'a t -> 'a

(** [in_same_domain actor] tells if you're running in [actor]'s domain. *)
val in_same_domain : 'a t -> bool
