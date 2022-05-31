(* An actor, 's is the type of the messages,
   'a is the return type of methods *)
type ('s, 'a) t = {
  fifo : ('s * 'a Promise.t) Queue.t;
  methods : 's -> 'a
}

(* send :: ('s, 'a) actor -> 's -> 'a future *)
let send actor message =
  let f = Promise.make_empty () in
  Promise.fill (fun () -> actor.methods message) f;
  f
