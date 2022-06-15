(* Module Oactor *)
  type 'a t = {
    scheduler : Roundrobin.t;
    methods : 'a;
    mutable domain : unit Domain.t Option.t
  }

  let stop self =
    match self.domain with
    | None -> failwith "Cannot stop non-running actor";
    | Some d ->
      Roundrobin.stop self.scheduler;
      self.domain <- None;
      try Domain.join d with
       | Roundrobin.Stop -> ()

  let run self =
    Gc.finalise stop self;
    let domain = Roundrobin.run self.scheduler in
    self.domain <- Some domain

  let create methods =
    let a = {
    scheduler = Roundrobin.create ();
    methods = methods;
    domain = None
  } in run a; a
