(* Module Oactor *)
type 'a t = {
  scheduler : Roundrobin.t;
  mutable methods : 'a Option.t;
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

let create methods =
  let self = {
    scheduler = Roundrobin.create ();
    methods = None;
    domain = None
  } in
  self.methods <- Some (methods self);
  Gc.finalise stop self;
  self
