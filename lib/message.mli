(** Module type for Messages (or methods) *)
module type S = sig
  (** Type of messages *)
  type 'a t

  (** Type of methods, used by the actor.
      We use a record to have universal quantification. *)
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end
