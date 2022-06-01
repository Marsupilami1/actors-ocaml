type 'a t
type _ Effect.t += NotReady : 'a t -> 'a Effect.t
exception Future__Multiple_Write
val create : unit -> 'a t
val get : 'a t -> 'a
val wait_and_get : 'a t -> 'a
val fill : 'a t -> 'a -> unit
val is_ready : 'a t -> bool
