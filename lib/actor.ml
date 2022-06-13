(* Module Actor *)
module Make(S : Scheduler.S)(M : Message.S) = struct
  type t = {
    scheduler : S.t;
    methods : t -> M.method_type;
    mutable domain : unit Domain.t Option.t
  }

  let stop self =
    match self.domain with
    | None -> failwith "Cannot stop non-running actor";
    | Some d ->
      S.stop self.scheduler;
      self.domain <- None;
      try Domain.join d with
       | S.Stop -> ()

  let run self =
    Gc.finalise stop self;
    let domain = S.run self.scheduler in
    self.domain <- Some domain

  let create methods =
    let a = {
    scheduler = S.create ();
    methods = methods;
    domain = None
  } in run a; a

  let send self message =
    let (p, fill) = Promise.create () in
    let forward p' = Promise.unify p p'; raise S.Interrupt in
    S.push_process self.scheduler (fun _ ->
        fill (((self.methods self).m) forward message));
    p

  let wait_for condition =
    S.wait_for condition
end


module Main = struct
  module MainMessage = struct
    type 'a t
    type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
  end
  module MainScheduler = struct
    include Roundrobin
    let run fifo = loop fifo; exit 0
  end

  include Make(MainScheduler)(MainMessage)

  let fifo = MainScheduler.create ()

  let run main =
    MainScheduler.push_process fifo (fun () ->
        main (); Gc.full_major (); raise MainScheduler.Stop);
    try MainScheduler.run fifo with
    | MainScheduler.Stop -> exit 0
end
