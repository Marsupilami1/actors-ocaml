(* Module Actor *)
module Make(S : Message.S) = struct
  open Effect.Deep

  exception Stop

  type process = (unit -> unit)

  (* Type of Actors, 'm is the type of memory *)
  type 'm t = {
    processes : process Domainslib.Chan.t;
    memory : 'm Domain.DLS.key;
    methods : 'm t -> S.method_type
  }

  let create init methods = {
    processes = Domainslib.Chan.make_unbounded ();
    memory = Domain.DLS.new_key init;
    methods = methods
  }

  let get_memory self = Domain.DLS.get self.memory
  let set_memory self memory = Domain.DLS.set self.memory memory

  let push_process self process =
    Domainslib.Chan.send self.processes process

  let get_process self =
    Domainslib.Chan.recv self.processes

  let send self message =
    let p = Promise.create () in
    push_process self (fun () ->
        Promise.fill p (((self.methods self).m) message));
    p

  let manage_next_process self =
    get_process self ()

  let rec loop self =
    match_with manage_next_process self {
      retc = (fun _ -> loop self);
      exnc = raise;
      effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Promise.NotReady p -> Some (
            fun (k : (a, _) continuation) ->
              (* The process is waiting for the promise to be filled *)
              (* So we add a hook to this promise to push the process *)
              (* back to the queue *)
              Promise.add_hook p (fun v ->
                  push_process self (fun _ -> continue k v));
              loop self;
          )
        | _ -> None
    }

  type running = unit Domain.t
  let run self = Domain.spawn (fun _ ->
      loop self
    )

  let stop self r =
    push_process self (fun _ -> raise Stop);
    try Domain.join r with
    | Stop -> ()
end
