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
    module E = Effect.Shallow
    exception Stop

    type process = Process : {
        r : 'a Promise.resolver ;
        k : ('b, 'a) E.continuation ;
        v : 'b ;
      } -> process
    type t = {processes : process Domainslib.Chan.t;}
    let push_process fifo process =
      Domainslib.Chan.send fifo.processes process
    let get_process fifo =
      Domainslib.Chan.recv fifo.processes

    type 'a action = Done of 'a | Push of ('a Promise.resolver -> unit)
    
    let launch pool fifo =
      let rec h = {E.
        retc = (fun v -> Done v);
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Multiroundrobin.Spawn -> Some (
              fun (k : (a, _) E.continuation) ->
                E.continue_with k (Multiroundrobin.create pool) h
            )
          | Promise.NotReady p -> Some (
              fun (k : (a, _) E.continuation) ->
                Push (fun r -> 
                    Promise.add_callback p (fun v ->
                        push_process fifo
                          (Process{ r; k; v}))
                  )
            )
          | Promise.Get p -> Some (
              fun (k : (a, _) E.continuation) ->
                while not (Promise.is_ready p) do
                  Domain.cpu_relax ()
                done;
                E.continue_with k (Promise.get p) h
            )
          | Promise.Async (r, f) -> Some (
              fun (k : (a, _) E.continuation) ->
                push_process fifo (Process{ r; k = E.fiber f; v = () });
                E.continue_with k () h
            )
          | _ -> None
      }
      in
      let rec loop () =
        let Process{r; k; v} = get_process fifo in
        match E.continue_with k v h with
        | Done v ->
          Promise.resolve r v;
          loop ()
        | Push f ->
          f r;
          loop ()
      in
      loop ()

    let create () =
      { processes = Domainslib.Chan.make_unbounded () },
      Domain.self (),
      Multiroundrobin.Pool.init ()
  end

  let run main =
    let fifo, _main_domain, pool = MainScheduler.create () in
    MainScheduler.push_process fifo
      (let r = Promise.never_resolve ()
       and k = Effect.Shallow.fiber (fun _ -> main (); raise MainScheduler.Stop)
       and v = () in
       Process {r;k;v});  
    (try MainScheduler.launch pool fifo with
     | MainScheduler.Stop -> ());
    Multiroundrobin.Pool.stop pool

end
