open Effect
open Effect.Deep

module Chan = struct
  type 'a t = 'a Queue.t
  let make_unbounded () = Queue.create ()
  let send t p = Queue.push p t
  let recv_poll t = Queue.take_opt t
  let peek_poll t = Queue.peek_opt t
end

exception Stop
exception Interrupt
type _ Stdlib.Effect.t += Forward : ('a Promise.resolver -> unit) -> 'a Stdlib.Effect.t

let spawned_actors = ref 0
let max_domains = 7 (* for 8 cores hardware *)

type process = Process : ('a Promise.resolver * (unit -> 'a)) -> process
type process_queue = process Chan.t
type trigger = Mutex.t * Condition.t

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

type actor_info = {fifo : process_queue; mutable current : process Option.t}

type domain_info = {
  (* data structure for accessing messages *)
  fifos : actor_info Queue.t;
  (* trigger to avoid spamming in get_process *)
  trigger : trigger;
  (* Domain in which the actor is runnning *)
  mutable domain : unit Domain.t Option.t;
}

type pool = domain_info array

type t = process_queue * trigger

type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
let wait_for condition =
  perform @@ WaitFor condition

type _ Effect.t += Yield : unit Effect.t
let yield () =
  perform @@ Yield

type 'a Effect.t += Spawn : (t * Domain.id) Effect.t                        

let push_process data process =
  let fifo, (mutex, condition) = data in
  Mutex.lock mutex;
  Chan.send fifo process;
  Mutex.unlock mutex;
  Condition.signal condition

let stop _id = ()

let create (pool : pool) : t * Domain.id =
  let id = !spawned_actors in
  incr spawned_actors;
  let index = id mod max_domains in
  let queue = Chan.make_unbounded () in
  let domain_info = pool.(index) in
  let mutex = fst domain_info.trigger in
  Mutex.lock mutex;
  Queue.push {fifo = queue; current = None} domain_info.fifos;
  Mutex.unlock mutex;
  ((queue, pool.(index).trigger),
   Domain.get_id @@ Option.get domain_info.domain)

let create_pool () : domain_info Array.t =
  Array.init max_domains (fun _ -> {
        fifos = Queue.create ();
        trigger = (Mutex.create (), Condition.create ());
        domain = None;
      }
    )

let spawn_pool pool = 
  Array.iter (fun info -> 
      let is_process () =
        (* print_endline "b1"; *)
        let b = Queue.fold (fun b actor_info ->
            b <|> actor_info.current <|> Chan.peek_poll actor_info.fifo
          ) None info.fifos <> None in
        (* print_endline "b2"; *)
        b
      in
      let get_next_process () =
        let mutex, condition = info.trigger in
        Mutex.lock mutex;
        (* TODO: merge both function *)
        (* block until a process is available *)
        while not (is_process ()) do Condition.wait condition mutex done;

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
      in
      let push_process queue process =
        let mutex, condition = info.trigger in
        Mutex.lock mutex;
        Chan.send queue process;
        Mutex.unlock mutex;
        Condition.signal condition
      in
      let rec loop () =
        let (Process (resolver, exec)), current_actor_info =
          get_next_process () in
        match_with exec () {
          retc = (fun v ->
              Promise.resolve resolver v;
              (loop [@tailcall]) ();
            );
          exnc = (fun e -> match e with
              | Interrupt -> (loop [@tailcall]) ()
              | Stop -> raise Stop
              | _ -> Promise.fail resolver e; (loop [@tailcall]) ()
            );
          effc = fun (type a) (e : a Effect.t) ->
            match e with
            | Spawn -> Some (
                fun (k : (a, _) continuation) ->
                  continue k (create pool)
              )
            | Promise.NotReady p -> Some (
                fun (k : (a, _) continuation) ->
                  (* The process is waiting for the promise to be filled *)
                  (* So we add a callback to this promise to push the process *)
                  (* back to the queue *)
                  Promise.add_callback p (fun v ->
                      push_process current_actor_info.fifo
                        (* The compiler is dumb *)
                        (Process(Obj.magic resolver, (fun _ -> continue k v)))
                    );
                  (loop [@tailcall]) ();
              )
            | Promise.Get p -> Some (
                fun (k : (a, _) continuation) ->
                  Mutex.lock @@ fst info.trigger;
                  current_actor_info.current <- Some
                      (Process(Obj.magic resolver, (fun _ -> continue k (Promise.get p))));
                  Mutex.unlock @@ fst info.trigger;
                  Condition.signal @@ snd info.trigger;
                  (loop [@tailcall]) ()
              )
            | Forward f -> Some (
                fun (k : (a, _) continuation) ->
                  f (Obj.magic resolver);
                  discontinue k Interrupt;
              )
            | Promise.Async (r, f) -> Some (
                fun (k : (a, _) continuation) ->
                  push_process current_actor_info.fifo (Process(r, f));
                  ignore @@ continue k ()
              )
            | WaitFor condition -> Some (
                fun (k : (a, _) continuation) ->
                  if condition () then
                    ignore @@ continue k ()
                  else (
                    push_process current_actor_info.fifo
                      (Process(Obj.magic resolver,
                               (fun _ -> wait_for condition; continue k ())));
                    (loop [@tailcall]) ()
                  )
              )
            | _ -> None
        } in
      let d = Domain.spawn (fun _ -> loop ()) in
      info.domain <- Some d;
    )
    pool

let init () =
  let pool = create_pool () in
  spawn_pool pool;
  pool

let stop_all (pool : pool) =
  for _ = 1 to Array.length pool do
    (* Stop the thread *)
    let stopping_actor, _ = create pool in
    push_process stopping_actor (Process(Promise.never_resolve (), (fun _ -> raise Stop)));
  done;
  for domain = 0 to Array.length pool - 1 do
    try Domain.join (Option.get @@ pool.(domain).domain) with
    | Stop -> ()
  done
