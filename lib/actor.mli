type ('s, 'a) t

val create : (('s, 'a) t -> ('s -> 'a)) -> ('s, 'a) t
val send : ('s, 'a) t -> 's -> 'a Promise.t

val run : ('s, 'a) t -> unit
