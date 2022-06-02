module Make (S : Message.S) : sig
  type 'm t

  val create : (unit -> 'm) -> ('m t -> S.method_type) -> 'm t
  val send : 'm t -> 'a S.t -> 'a Promise.t
  val get_memory : 'm t -> 'm
  val set_memory : 'm t -> 'm -> unit

  val run : 'm t -> unit
end
