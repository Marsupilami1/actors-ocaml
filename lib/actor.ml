(* Module actor *)

type 'a t = Actor of 'a
  constraint 'a = <scheduler : Multiroundrobin.t; domain : Domain.id; ..> [@@ocaml.unboxed]

let send (Actor actor) process =
  Multiroundrobin.push_process actor#scheduler process

let methods (Actor actor) = actor

let in_same_domain (Actor actor) =
  actor#domain = Domain.self ()

let forward p =
  Effect.perform @@ Multiroundrobin.Forward (fun fill -> Promise.add_callback p fill)


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

    let rec loop fifo =
      let Process(fill, exec) = get_process fifo in
      match_with exec () {
        retc = (fun v -> fill v; loop fifo);
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Promise.NotReady p -> Some (
              fun (k : (a, _) continuation) ->
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
          | Promise.Async f -> Some (
              fun (k : (a, _) continuation) ->
                push_process fifo (Process(ignore, f));
                ignore @@ continue k ()
            )
          | _ -> None
      }
    let create () = {
      processes = Domainslib.Chan.make_unbounded ();
    }, Domain.self ()
    let stop fifo =
      push_process fifo (Process(ignore, (fun _ -> Gc.full_major (); raise Stop)));
  end

  let run main =
    let fifo = fst @@ MainScheduler.create () in
    MainScheduler.push_process fifo (Process(ignore, (fun _ ->
        main (); MainScheduler.stop fifo)));
    (try MainScheduler.loop fifo with
     | MainScheduler.Stop -> ());
    Multiroundrobin.stop_all ()

end
