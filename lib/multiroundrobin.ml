
module Chan = struct
  type 'a t = 'a Queue.t
  let make_unbounded () = Queue.create ()
  let send t p = Queue.push p t
  let recv_poll t = Queue.take_opt t
  let peek_poll t = Queue.peek_opt t
end


let spawned_actors = ref 0
let max_domains = 7 (* for 8 cores hardware *)


module E = Effect.Shallow
type process =
  | Process : {
      r : 'a Promise.resolver ;
      k : ('b, 'a) E.continuation ;
      v : 'b ;
    } -> process
  | Discontinue : {
      r : 'a Promise.resolver ;
      k : ('b, 'a) E.continuation ;
      exn : exn ;
    } -> process
[@@warning "-37"]

let process r f =
  Process {r; k = E.fiber f; v = ()}

type process_queue = process Chan.t
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
  
type _ Stdlib.Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Stdlib.Effect.t

type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
let wait_for condition =
  Effect.perform @@ WaitFor condition

type _ Effect.t += Yield : unit Effect.t
let yield () =
  Effect.perform @@ Yield

type 'a Effect.t += Spawn : (t * Domain.id) Effect.t        

(** Scheduler utilities inside a domain *)

module SchedulerDomain = struct

  type actor_info = {fifo : process_queue; mutable current : process Option.t}

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
        b <|> actor_info.current <|> Chan.peek_poll actor_info.fifo
      ) None info.fifos <> None in
    (* print_endline "b2"; *)
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
    Condition.signal condition;

  
  type 'a action =
    | Pass
    | Return of 'a
    | Do of ('a Promise.resolver -> actor_info -> 'a action)
    | Fail of exn

  let rec launch get_domain info =
    let retc v = Return v in
    let exnc e = Fail e in
    let rec h =
      { E. retc; exnc; effc = fun (type a) (e : a Effect.t) ->
            match e with 
            | Spawn -> Some (
                fun (k : (a, _) E.continuation) ->
                  E.continue_with k (create get_domain) h
              )
            | Promise.NotReady p -> Some (
                fun (k : (a, _) E.continuation) ->
                  (* The process is waiting for the promise to be filled *)
                  (* So we add a callback to this promise to push the process *)
                  (* back to the queue *)
                  Do (fun r current_actor_info ->
                      Promise.add_callback p (fun v ->
                          push_process info current_actor_info.fifo
                            (* The compiler is dumb *)
                            (Process{r ; k; v})
                        );
                      Pass)
              )
            (* | Promise.Get p -> Some (
             *     fun (k : (a, _) E.continuation) ->
             *       Do (fun r current_actor_info ->
             *           let k =
             *             E.fiber (fun () ->
             *                 loop_on r current_actor_info
             *                   (E.fiber Promise.get) @@ Obj.magic p)
             *           in
             *           let process = Process {r; k ; v = ()} in
             *           set_current info current_actor_info process;
             *           Pass
             *         )
             *   ) *)
            | Forward f -> Some (
                fun (k : (a, _) E.continuation) ->
                  Do (fun r _ ->
                      f @@ Obj.magic r;
                      E.discontinue_with k Interrupt h
                    )
              )
            | Promise.Async (r, f) -> Some (
                fun (k : (a, _) E.continuation) ->
                  Do (fun _ current_actor_info ->
                      push_process info current_actor_info.fifo
                        (Process{r; k = E.fiber f; v = ()});
                      E.continue_with k () h
                    )
              )
            (* | WaitFor condition -> Some (
             *     fun (k : (a, _) E.continuation) ->
             *       Do (fun r current_actor_info ->
             *           if condition () then
             *             E.continue_with k () h
             *           else (
             *             push_process info current_actor_info.fifo
             *               (Process{r; ,
             *                        (fun _ -> wait_for condition; continue k ())));
             *             (loop [@tailcall]) ()
             *           )
             *         )
             *   ) *)
            | _ -> None}
    in

    let rec loop () =
      let process, current_actor_info = get_next_process info in
      let rec exec_action r current_actor_info = function
        | Pass ->
          (loop [@tailcall]) ()        
        | Return v ->
          Promise.resolve r v;
          (loop [@tailcall]) ()
        | Do f ->
          exec_action r current_actor_info @@ f r current_actor_info
        | Fail e -> begin match e with
            | Interrupt -> (loop [@tailcall]) ()
            | Stop -> raise Stop
            | _ -> Promise.fail r e; (loop [@tailcall]) ()
          end
      in 
      match process with
      | Process{r; k; v} ->
        exec_action r current_actor_info @@ E.continue_with k v h
      | Discontinue{r; k; exn} ->
        Promise.fail r exn;
        exec_action r current_actor_info @@ E.discontinue_with k exn h
    in
    loop ()

  and create get_domain : t * Domain.id =
    let id = !spawned_actors in
    incr spawned_actors;
    let index = id mod max_domains in
    let queue = Chan.make_unbounded () in
    let domain_info = get_domain index in
    let mutex = fst domain_info.trigger in
    Mutex.lock mutex;
    Queue.push {fifo = queue; current = None} domain_info.fifos;
    Mutex.unlock mutex;
    ((queue, domain_info.trigger),
     Domain.get_id @@ Option.get domain_info.domain)


  let stop _id = ()

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
         and k = Effect.Shallow.fiber (fun _ -> raise Stop)
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
              
