include Scheduler.S

module Pool : sig

  type t = pool

  val init : unit -> pool

  val stop : pool -> unit

end
