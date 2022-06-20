(** The type of the actor. *)
type 'a t = {
  scheduler : Roundrobin.t;
  mutable methods : 'a Option.t;
  mutable domain : unit Domain.t Option.t
}

(** [create methods] returns an actor with methods [methods]. *)
val create : ('a t -> 'a) -> 'a t
