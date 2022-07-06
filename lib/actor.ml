(* Module actor *)

type 'a t = Actor of 'a
  constraint 'a = <scheduler : Multiroundrobin.t; domain : Domain.id; ..> [@@ocaml.unboxed]

let send (Actor actor) process =
  Multiroundrobin.push_process actor#scheduler process

let methods (Actor actor) = actor

let in_same_domain (Actor actor) =
  actor#domain = Domain.self ()

let forward p =
  Effect.perform @@ Multiroundrobin.Unify p

let spawn () = Effect.perform Multiroundrobin.Spawn

module Main = struct
  module MainScheduler = struct
    module E = Effect.Deep
    exception Stop

    type 'a action = Done of 'a | Push of ('a Promise.resolver -> unit)

    type ('b, 'a) process_desc =
      | Cont of ('b, 'a action) E.continuation
      | Fun of ('b -> 'a)

    type process = Process : {
        r : 'a Promise.resolver ;
        k : ('b, 'a) process_desc ;
        v : 'b ;
      } -> process

    type t = {processes : process Domainslib.Chan.t;}
    let push_process fifo process =
      Domainslib.Chan.send fifo.processes process
    let get_process fifo =
      Domainslib.Chan.recv fifo.processes


    let launch pool fifo =
      let h = {E.
        retc = (fun v -> Done v);
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Multiroundrobin.Spawn -> Some (
              fun (k : (a, _) E.continuation) ->
                E.continue k (Multiroundrobin.create pool)
            )
          | Promise.NotReady p -> Some (
              fun (k : (a, _) E.continuation) ->
                Push (fun r -> 
                    Promise.add_callback p (fun v ->
                        push_process fifo
                          (Process{ r; k = Cont k; v}))
                  )
            )
          | Promise.Get p -> Some (
              fun (k : (a, _) E.continuation) ->
                while not (Promise.is_ready p) do
                  Domain.cpu_relax ()
                done;
                E.continue k (Promise.get p)
            )
          | Promise.Async (r, f) -> Some (
              fun (k : (a, _) E.continuation) ->
                push_process fifo (Process{ r; k = Fun f; v = () });
                E.continue k ()
            )
          | _ -> None
      }
      in
      let rec loop () =
        let Process{r; k; v} = get_process fifo in
        let action = match k with
        | Cont k -> E.continue k v
        | Fun f -> E.match_with f v h
        in
        match action with
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
       and k = MainScheduler.Fun (fun _ -> main (); raise MainScheduler.Stop)
       and v = () in
       Process {r;k;v});
    (try MainScheduler.launch pool fifo with
     | MainScheduler.Stop -> ());
    Multiroundrobin.Pool.stop pool

end
