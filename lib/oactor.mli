(** The type of the actor. *)
type 'a t = {
  scheduler : Roundrobin.t;
  methods : 'a;
  mutable domain : unit Domain.t Option.t
}

(** [create methods] returns an actor with methods [methods]. *)
val create : 'a -> 'a t
