include Scheduler.S

type _ Stdlib.Effect.t += Unify : 'a Promise.t -> 'a Stdlib.Effect.t

module Pool : sig

  type t = pool

  val init : unit -> pool

  val stop : pool -> unit

end
