(* Module actor *)

type 'a t = Actor of 'a
  constraint 'a = <scheduler : Multiroundrobin.t; domain : Domain.id; ..> [@@ocaml.unboxed]

let send (Actor actor) process =
  Multiroundrobin.push_process actor#scheduler process

let methods (Actor actor) = actor

let in_same_domain (Actor actor) =
  actor#domain = Domain.self ()

let forward p =
  Effect.perform @@ Multiroundrobin.Forward (fun resolver -> Promise.add_callback p (Promise.resolve resolver))

let spawn () = Effect.perform Multiroundrobin.Spawn

module Main = struct
  module MainScheduler = struct
    open Effect.Deep
    exception Stop

    type process = Process : ('a Promise.resolver * (unit -> 'a)) -> process
    type t = {processes : process Domainslib.Chan.t;}
    let push_process fifo process =
      Domainslib.Chan.send fifo.processes process
    let get_process fifo =
      Domainslib.Chan.recv fifo.processes

    (* TODO: avoid handler stacking *)
    let rec loop pool fifo =
      let Process(r, exec) = get_process fifo in
      match_with exec () {
        retc = (fun v ->
            Promise.resolve r v;
            (loop [@tailcall]) pool fifo);
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Multiroundrobin.Spawn -> Some (
              fun (k : (a, _) continuation) ->
                continue k (Multiroundrobin.create pool)
            )
          | Promise.NotReady p -> Some (
              fun (k : (a, _) continuation) ->
                Promise.add_callback p (fun v ->
                    push_process fifo (Process(Obj.magic r, (fun _ -> continue k v))));
                (loop [@tailcall]) pool fifo;
            )
          | Promise.Get p -> Some (
              fun (k : (a, _) continuation) ->
                while not (Promise.is_ready p) do
                  Domain.cpu_relax ()
                done;
                ignore @@ continue k (Promise.get p);
                (loop [@tailcall]) pool fifo;
            )
          | Promise.Async (r, f) -> Some (
              fun (k : (a, _) continuation) ->
                push_process fifo (Process(r, f));
                ignore @@ continue k ()
            )
          | _ -> None
      }
    let create () =
      { processes = Domainslib.Chan.make_unbounded () },
      Domain.self (),
      Multiroundrobin.init ()
  end

  let run main =
    let fifo, _main_domain, pool = MainScheduler.create () in
    MainScheduler.push_process fifo (Process(Promise.never_resolve (), (fun _ ->
        main (); raise MainScheduler.Stop)));
    (try MainScheduler.loop pool fifo with
     | MainScheduler.Stop -> ());
    Multiroundrobin.stop_all pool

end
