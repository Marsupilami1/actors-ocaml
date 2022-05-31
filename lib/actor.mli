(* Type of actors: messages 's and return type 'a ('a should be a fantom type in 's) *)
type ('s, 'a) t = {
  fifo : ('s * 'a Promise.t) Queue.t;
  methods : 's -> 'a
}
val send : ('s, 'a) t -> 's -> 'a Promise.t
