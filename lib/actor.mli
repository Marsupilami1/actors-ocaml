type ('m, 's, 'a) t

val create : (unit -> 'm) -> (('m, 's, 'a) t -> ('s -> 'a)) -> ('m, 's, 'a) t
val send : ('m, 's, 'a) t -> 's -> 'a Promise.t
val get_memory : ('m, 's, 'a) t -> 'm
val set_memory : ('m, 's, 'a) t -> 'm -> unit

val run : ('m, 's, 'a) t -> unit
