module Make (S : Message.S) : sig
  type 'm t

  val create : (unit -> 'm) -> ('m t -> S.method_type) -> 'm t
  val async : 'm t -> (unit -> 'a) -> 'a Promise.t
  val send : 'm t -> 'a S.t -> 'a Promise.t
  val get_memory : 'm t -> 'm
  val set_memory : 'm t -> 'm -> unit
  val wait_for : (unit -> bool) -> unit

  type running
  val run : 'm t -> running
  val stop : 'm t -> running -> unit
end
