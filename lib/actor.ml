type signal = Welcome of string

type ('s, 'a) t = {fifo : ('s * 'a Promise.t) Queue.t; methods : 's -> 'a}

let send message actor =
    let p = Promise.make_empty () in
    Queue.push (message, p) actor.fifo;
    p

let call_method actor (message, promise) =
    Promise.fork ((fun _ -> actor.methods message), promise)

let run () =
    let actor = {fifo = Queue.create (); methods = fun (Welcome s) -> "Bonjour " ^ s} in
    let f = send (Welcome "Martin") actor in (* client code *)
    if not @@ Queue.is_empty actor.fifo then begin (* actor loop *)
        let message = Queue.take actor.fifo in
        call_method actor message;
    end;
    print_endline @@ Promise.get_val f
