open Effect
open Effect.Deep

module Chan = Domainslib.Chan

exception Stop
exception Interrupt

let spawned_actors = ref 0
let max_domains = 1 (* for 8 cores hardware *)

type process = unit -> unit
type process_queue = process Chan.t
type trigger = Mutex.t * Condition.t

module Queue = struct
  include Queue

  (** [rotate q] returns the first value in [q] and push it back to [q]. *)
  let rotate q =
    let v = Queue.take q in
    Queue.push v q;
    v
end

type domain_info = {
  (* data structure for accessing messages *)
  fifos : process_queue Queue.t;
  (* trigger to avoid spamming in get_process *)
  trigger : trigger;
  (* process count *)
  p_count : int Atomic.t;
  (* Domain in which the actor is runnning *)
  mutable domain : unit Domain.t Option.t;
}

type t = process_queue * trigger * int Atomic.t

type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
let wait_for condition =
  perform @@ WaitFor condition

type _ Effect.t += Yield : unit Effect.t
let yield () =
  perform @@ Yield


let domains : domain_info Array.t =
  Array.init max_domains (fun _ ->
      let info = {
        fifos = Queue.create ();
        trigger = (Mutex.create (), Condition.create ());
        p_count = Atomic.make 0;
        domain = None;
      }
      in

      let get_next_process () =
        let mutex, condition = info.trigger in
        Mutex.lock mutex;
        let p_count = Atomic.get info.p_count in
        if p_count = 0 then
          Condition.wait condition mutex;
        (* info.p_count is not 0 *)
        Atomic.decr info.p_count;
        (* find the next process to execute *)
        let res = ref None in
        let current_actor_queue = ref @@ Queue.peek info.fifos in
        while !res = None do
          current_actor_queue := Queue.peek info.fifos;
          res := Chan.recv_poll @@ Queue.rotate info.fifos;
        done;
        Mutex.unlock mutex;
        Option.get !res, !current_actor_queue
      in

      let push_process queue process =
        let mutex, condition = info.trigger in
        Mutex.lock mutex;
        Chan.send queue process;
        Atomic.incr info.p_count;
        Mutex.unlock mutex;
        Condition.broadcast condition;
      in

      let rec loop () =
        let process, current_actor_queue = get_next_process () in
        match_with process () {
          retc = (fun _ -> loop ());
          exnc = (fun e -> match e with
              | Interrupt -> loop ()
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
                      push_process current_actor_queue (fun _ -> continue k v));
                  loop ();
              )
            | Promise.Async f -> Some (
                fun (k : (a, _) continuation) ->
                  push_process current_actor_queue f;
                  continue k ()
              )
            | WaitFor condition -> Some (
                fun (k : (a, _) continuation) ->
                  if condition () then
                    continue k ()
                  else (
                    push_process current_actor_queue (fun _ ->
                        wait_for condition; continue k ());
                    loop ()
                  )
              )
            | _ -> None
        } in
      let d = Domain.spawn (fun _ -> loop ()) in
      info.domain <- Some d;
      info
    )

let push_process data process =
  let fifo, (mutex, condition), p_count = data in
  Mutex.lock mutex;
  Chan.send fifo process;
  Atomic.incr p_count;
  Mutex.unlock mutex;
  Condition.broadcast condition


let stop _id = ()

let create () =
  let id = !spawned_actors in
  incr spawned_actors;
  let index = id mod max_domains in
  let queue = Chan.make_unbounded () in
  let domain_info = domains.(index) in
  let mutex = fst domain_info.trigger in
  Mutex.lock mutex;
  Queue.push queue domain_info.fifos;
  Mutex.unlock mutex;
  queue, domains.(index).trigger, domains.(index).p_count

let stop_all () =
  (* for domain = 0 to max_domains - 1 do *)
  (*   empty_fifos domain; *)
  (* done; *)
  for _ = 1 to max_domains do
    (* Clear the process queue *)
    (* Stop the thread *)
    let stopping_actor = create () in
    push_process stopping_actor (fun _ -> raise Stop);
  done;
  for domain = 0 to max_domains - 1 do
    try Domain.join (Option.get @@ domains.(domain).domain) with
    | Stop -> ()
  done
