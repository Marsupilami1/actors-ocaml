(* Module Actor *)
module Make(S : Scheduler.S)(M : Message.S) = struct
  (* Type of Actors, 'm is the type of memory *)
  type 'm t = {
    scheduler : S.t;
    memory : 'm Domain.DLS.key;
    methods : 'm t -> M.method_type;
    mutable domain : unit Domain.t Option.t
  }

  let create init methods = {
    scheduler = S.create ();
    memory = Domain.DLS.new_key init;
    methods = methods;
    domain = None
  }

  let get_memory self = Domain.DLS.get self.memory
  let set_memory self memory = Domain.DLS.set self.memory memory

  let async self f =
    let p = Promise.create () in
    S.push_process self.scheduler (fun _ -> Promise.fill p (f ()));
    p

  let send self message =
    let p = Promise.create () in
    let forward p' = Promise.unify p p'; raise S.Interrupt in
    S.push_process self.scheduler (fun _ ->
        Promise.fill p (((self.methods self).m) forward message));
    p

  let wait_for condition =
    S.wait_for condition


  let run self =
    let domain = S.run self.scheduler in
    self.domain <- Some domain

  let stop self =
    match self.domain with
    | None -> failwith "Cannot stop non-running actor";
    | Some d ->
      S.stop self.scheduler;
      (try Domain.join d with
       | S.Stop -> ()
      );

end
