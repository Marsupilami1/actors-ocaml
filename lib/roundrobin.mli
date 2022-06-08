exception Stop
exception Interrupt

type t
type process = unit -> unit

val create : unit -> t
val push_process : t -> process -> unit
val wait_for : (unit -> bool) -> unit
val run : t -> unit Domain.t
val stop : t -> unit
