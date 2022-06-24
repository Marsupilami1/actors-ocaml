(* Module Actor *)
module Make(S : Scheduler.S)(M : Message.S) = struct
  type t = {
    scheduler : S.t;
    methods : t -> M.method_type;
  }

  let stop self =
    S.stop self.scheduler

  let create methods =
    let a = {
      scheduler = fst @@ S.create ();
      methods = methods;
    } in
    Gc.finalise stop a;
    a

  let send self message =
    let (p, fill) = Promise.create () in
    let forward p' = Promise.unify p p'; raise S.Interrupt in
    S.push_process self.scheduler @@ Process(fill, (fun _ ->
        ((self.methods self).m) forward message));
    p

  let wait_for condition =
    S.wait_for condition

  let yield () =
    S.yield ()
end

module DefaultActor = Make(Multiroundrobin)


module Main = struct
  module MainMessage = struct
    type 'a t
    type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
  end
  module MainScheduler = struct
    open Effect
    open Effect.Deep
    exception Stop
    exception Interrupt
    type _ Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Effect.t
    type process = Process : ('a Promise.resolver * (unit -> 'a)) -> process
    type t = {processes : process Domainslib.Chan.t;}
    let push_process fifo process =
      Domainslib.Chan.send fifo.processes process
    let get_process fifo =
      Domainslib.Chan.recv fifo.processes
    type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
    let wait_for condition =
      perform @@ WaitFor condition
    type _ Effect.t += Yield : unit Effect.t
    let yield () =
      perform @@ Yield
    let rec loop fifo =
      let Process(fill, exec) = get_process fifo in
      match_with exec () {
        retc = (fun v -> fill v; loop fifo);
        exnc = (fun e -> match e with
            | Interrupt -> loop fifo
            | _ -> raise e
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Promise.NotReady p -> Some (
              fun (k : (a, _) continuation) ->
                (* The process is waiting for the promise to be filled *)
                (* So we add a callback to this promise to push the process *)
                (* back to the queue *)
                Promise.add_callback p (fun v ->
                    push_process fifo (Process(Obj.magic fill, (fun _ -> continue k v))));
                loop fifo;
            )
          | Promise.Get p -> Some (
              fun (k : (a, _) continuation) ->
                while not (Promise.is_ready p) do
                  Domain.cpu_relax ()
                done;
                ignore @@ continue k (Promise.get p);
                loop fifo;
            )
          | Forward _ -> Some (
              fun (k : (a, _) continuation) ->
                discontinue k (Failure "Cannot use forward in Main actor.");
            )
          | Promise.Async f -> Some (
              fun (k : (a, _) continuation) ->
                push_process fifo (Process(ignore, f));
                ignore @@ continue k ()
            )
          | WaitFor condition -> Some (
              fun (k : (a, _) continuation) ->
                if condition () then
                  ignore @@ continue k ()
                else (
                  push_process fifo (Process(Obj.magic fill, (fun _ ->
                      wait_for condition; continue k ())));
                  loop fifo
                )
            )
          | _ -> None
      }
    let create () = {
      processes = Domainslib.Chan.make_unbounded ();
    }, Domain.self ()
    let stop fifo =
      push_process fifo (Process(ignore, (fun _ -> Gc.full_major (); raise Stop)));
  end

  include Make(MainScheduler)(MainMessage)

  let run main =
    let fifo = fst @@ MainScheduler.create () in
    MainScheduler.push_process fifo (Process(ignore, (fun _ ->
        main (); MainScheduler.stop fifo)));
    (try MainScheduler.loop fifo with
     | MainScheduler.Stop -> ());
    Multiroundrobin.stop_all ()

end
