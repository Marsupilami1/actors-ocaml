type ('m, 's, 'a) t

val create : 'm -> (('m, 's, 'a) t -> ('s -> 'a)) -> ('m, 's, 'a) t
val send : ('m, 's, 'a) t -> 's -> 'a Promise.t
val memory : ('m, 's, 'a) t -> 'm

val run : ('m, 's, 'a) t -> unit
