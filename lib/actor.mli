type ('s, 'a) t (* Type of actors: messages 's and return type 'a ('a should be a fantom type in 's) *)
val send : 's -> ('s, 'a) t -> 'a Promise.t
val run : unit -> unit
