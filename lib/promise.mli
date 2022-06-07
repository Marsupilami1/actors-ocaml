type 'a t
type _ Effect.t += NotReady : 'a t -> 'a Effect.t
exception Future__Multiple_Write
val create : unit -> 'a t
val await : 'a t -> 'a
val get : 'a t -> 'a
val fill : 'a t -> 'a -> unit
val is_ready : 'a t -> bool

val add_callback : 'a t -> ('a -> unit) -> unit

val replace : 'a t -> 'a t -> unit
val fmap : ('a -> 'b) -> 'a t -> 'b t
val pure : 'a -> 'a t
val join : 'a t t -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t

module Infix : sig
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
end
