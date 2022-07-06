
module Chan = struct
  type 'a t = 'a Queue.t
  let make_unbounded () = Queue.create ()
  let send t p = Queue.push p t
  let recv_poll t = Queue.take_opt t
  let peek_poll t = Queue.peek_opt t
end


let spawned_actors = ref 0
let max_domains = 7 (* for 8 cores hardware *)


module E = Effect.Deep

type process =
  | Process : {
      r : 'a Promise.resolver ;
      k : ('b, 'a) process_desc ;
      v : 'b ;
    } -> process

and ('b, 'a) process_desc =
  | Cont of ('b, 'a action) E.continuation
  | Fun of ('b -> 'a)

and process_queue = process Chan.t

and actor_info = {
  fifo : process_queue;
  mutable current : process Option.t;
  mutable running : bool
}

and 'a action =
  | Pass
  | Return of 'a
  | Do of ('a Promise.resolver -> actor_info -> 'a action)
  | Fail of exn

let process r f =
  Process {r; k = Fun f; v = ()}

type trigger = Mutex.t * Condition.t
type t = process_queue * trigger

module Queue = struct
  include Queue

  (** [rotate q] returns the first value in [q] and push it back to [q]. *)
  let rotate q =
    let v = Queue.take q in
    Queue.push v q
end


let (<|>) a b =
  match a with
  | None -> b
  | _ -> a

(** Effects and Signals *)

exception Stop
exception Interrupt

(* type _ Stdlib.Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Stdlib.Effect.t *)
type _ Stdlib.Effect.t += Forward : (unit -> 'a) -> 'a Stdlib.Effect.t
type _ Stdlib.Effect.t += Unify : 'a Promise.t -> 'a Stdlib.Effect.t

type _ Effect.t += Yield : unit Effect.t
let yield () =
  Effect.perform @@ Yield

let wait_for condition =
  while Fun.negate condition () do
    yield ()
  done


type 'a Effect.t += Spawn : (t * Domain.id) Effect.t

(** Scheduler utilities inside a domain *)

module SchedulerDomain = struct

  type domain_info = {
    (* data structure for accessing messages *)
    fifos : actor_info Queue.t;
    (* trigger to avoid spamming in get_process *)
    trigger : trigger;
    (* Domain in which the actor is runnning *)
    mutable domain : unit Domain.t Option.t;
  }

  let is_process info =
    (* print_endline "b1"; *)
    let b = Queue.fold (fun b actor_info ->
        b <|> (if actor_info.running then
                 actor_info.current <|> Chan.peek_poll actor_info.fifo
               else None))
        None info.fifos <> None in
    b

  let get_next_process info =
    let mutex, condition = info.trigger in
    Mutex.lock mutex;
    (* TODO: merge both function *)
    (* block until a process is available *)
    while not (is_process info) do Condition.wait condition mutex done;
    (* find the next process to execute *)
    let rec find_process () =
      let current_actor_info = Queue.peek info.fifos in
      (* current process, stopped by get *)
      if current_actor_info.running then begin
        if current_actor_info.current <> None then begin
          let res = Option.get current_actor_info.current in
          current_actor_info.current <- None;
          (res, current_actor_info)
        end
        (* next process in the queue *)
        else begin
          match Chan.recv_poll @@ current_actor_info.fifo with
          | None -> Queue.rotate info.fifos; find_process ()
          | Some res -> (res, current_actor_info)
        end
      end else begin
        Queue.rotate info.fifos; find_process ()
      end
    in
    let res = find_process () in
    Queue.rotate info.fifos;
    Mutex.unlock mutex;
    res

  let push_process info queue process =
    let mutex, condition = info.trigger in
    Mutex.lock mutex;
    Chan.send queue process;
    Mutex.unlock mutex;
    Condition.signal condition

  let _set_current info current_actor_info process =
    let mutex, condition = info.trigger in
    Mutex.lock mutex;
    current_actor_info.current <- Some process;
    Mutex.unlock mutex;
    Condition.signal condition

  (** Domain creation and destruction *)

  let create get_domain : t * Domain.id =
    let id = !spawned_actors in
    incr spawned_actors;
    let index = id mod max_domains in
    let queue = Chan.make_unbounded () in
    let domain_info = get_domain index in
    let mutex = fst domain_info.trigger in
    Mutex.lock mutex;
    Queue.push {fifo = queue; current = None; running = true} domain_info.fifos;
    Mutex.unlock mutex;
    ((queue, domain_info.trigger),
     Domain.get_id @@ Option.get domain_info.domain)

  let stop _id = ()

  (** Main loop *)

  type univ_handler = { h : 'a . ('a, 'a action) E.handler }[@@ocaml.unboxed]
  let mk_handler get_domain info =
    let retc v = Return v in
    let exnc e = Fail e in
    let effc
      : type a b. a Effect.t -> ((a, b action) E.continuation -> b action) option
      = fun e ->
        match e with
        | Spawn -> Some (
            fun k ->
              E.continue k (create get_domain)
          )
        | Yield -> Some (
            fun k ->
              Do (fun r current_actor_info ->
                  push_process info current_actor_info.fifo
                    (Process{r; k = Cont k; v = ()});
                  Pass
                )
          )
        | Promise.NotReady p -> Some (
            fun k ->
              (* The process is waiting for the promise to be filled *)
              (* So we add a callback to this promise to push the process *)
              (* back to the queue *)
              Do (fun r current_actor_info ->
                  Promise.add_callback p (fun v ->
                      push_process info current_actor_info.fifo
                        (Process{r ; k = Cont k; v})
                    );
                  Pass)
          )
        | Promise.Get p -> Some (
            fun k ->
              Do (fun r current_actor_info ->
                  current_actor_info.running <- false;
                  Promise.add_callback p (fun v ->
                      current_actor_info.running <- true;
                      let process = Process { r ; k = Cont k ; v } in
                      _set_current info current_actor_info process;
                    );
                  Pass
                )
          )
        | Forward f -> Some (
            fun k ->
              Do (fun r current_actor_info ->
                  push_process info current_actor_info.fifo
                    (Process{r; k = Fun (Obj.magic f); v = ()});
                  E.discontinue k Interrupt
                )
          )
        | Unify p -> Some (
            fun k ->
              Do (fun r _ ->
                  Promise.add_callback p (fun v -> Promise.resolve r (Obj.magic v));
                  E.discontinue k Interrupt
                )
          )
        | Promise.Async (r, f) -> Some (
            fun (k : (a, _) E.continuation) ->
              Do (fun _ current_actor_info ->
                  push_process info current_actor_info.fifo
                    (Process{r; k = Fun f; v = ()});
                  E.continue k ()
                )
          )
        | _ -> None
    in
    { h = { E. retc; exnc; effc }}

  let rec exec_action
    : 'a .
      domain_info -> actor_info ->
      univ_handler -> 'a Promise.resolver -> 'a action -> unit
    = fun info current_actor_info h r action -> match action with
      | Pass ->
        (loop [@tailcall]) info h
      | Return v ->
        Promise.resolve r v;
        (loop [@tailcall]) info h
      | Do f ->
        (exec_action [@tailcall])
          info current_actor_info
          h r (f r current_actor_info)
      | Fail e -> begin match e with
          | Interrupt -> (loop [@tailcall]) info h
          | Stop -> raise Stop
          | _ -> Promise.fail r e; (loop [@tailcall]) info h
        end
  and loop info h : unit =
    let Process{r; k; v}, current_actor_info = get_next_process info in
    let action = match k with
      | Cont k -> E.continue k v
      | Fun f -> E.match_with f v h.h
    in
    (exec_action [@tailcall]) info current_actor_info h r action

  let launch get_domain info =
    let h0 = mk_handler get_domain info in
    loop info h0

end

(** Exported version *)
let push_process data process =
  let fifo, (mutex, condition) = data in
  Mutex.lock mutex;
  Chan.send fifo process;
  Mutex.unlock mutex;
  Condition.signal condition

module Pool = struct

  type t = SchedulerDomain.domain_info array

  let create () : t =
    Array.init max_domains (fun _ ->
        {SchedulerDomain.
          fifos = Queue.create ();
          trigger = (Mutex.create (), Condition.create ());
          domain = None;
        }
      )

  let get_domain = Array.get

  let spawn_pool pool =
    Array.iter (fun info ->
        let d = Domain.spawn (fun _ ->
            SchedulerDomain.launch (get_domain pool) info)
        in
        info.domain <- Some d;
      )
      pool

  let init () =
    let pool = create () in
    spawn_pool pool;
    pool

  let stop (pool : t) =
    for _ = 1 to Array.length pool do
      (* Stop the thread *)
      let stopping_actor, _ = SchedulerDomain.create (get_domain pool) in
      push_process stopping_actor
        (let r = Promise.never_resolve ()
         and k = Fun (fun _ -> raise Stop)
         and v = () in
         Process {r;k;v});
    done;
    for domain = 0 to Array.length pool - 1 do
      try Domain.join (Option.get @@ pool.(domain).domain) with
      | Stop -> ()
    done

end

type pool = Pool.t

let create pool = SchedulerDomain.create @@ Pool.get_domain pool
let stop = SchedulerDomain.stop
