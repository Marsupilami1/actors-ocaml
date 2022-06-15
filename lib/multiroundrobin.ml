open Effect
open Effect.Deep

exception Stop
exception Interrupt

let spawned_actors = ref 0
let max_domains = 127

type process = unit -> unit

type domain_info = {
  fifo : process Domainslib.Chan.t;
  mutable domain : unit Domain.t Option.t;
  mutable running_actors_count : int;
}

let domains : domain_info Option.t Array.t =
  Array.make max_domains None

type t = int

let get_domain_info id =
  let k = Option.get domains.(id mod max_domains) in
  k

let get_fifo id =
  (get_domain_info id).fifo


let push_process id process =
  Domainslib.Chan.send (get_fifo id) process

let get_process id =
  Domainslib.Chan.recv (get_fifo id)

let manage_next_process id =
  get_process id ()

type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
let wait_for condition =
  perform @@ WaitFor condition

type _ Effect.t += Yield : unit Effect.t
let yield () =
  perform @@ Yield

let rec loop id =
  match_with manage_next_process id {
    retc = (fun _ -> loop id);
    exnc = (fun e -> match e with
        | Interrupt -> loop id
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
                push_process id (fun _ -> continue k v));
            loop id;
        )
      | Promise.Async f -> Some (
          fun (k : (a, _) continuation) ->
            push_process id f;
            continue k ()
        )
      | WaitFor condition -> Some (
          fun (k : (a, _) continuation) ->
            if condition () then
              continue k ()
            else (
              push_process id (fun _ ->
                  wait_for condition; continue k ());
              loop id
            )
        )
      | _ -> None
  }

let stop id =
  push_process id (fun _ -> Gc.full_major (); raise Stop)

let create () =
  let id = !spawned_actors in
  if id < max_domains then begin
    domains.(id) <- Some {
        fifo = Domainslib.Chan.make_unbounded ();
        domain = None;
        running_actors_count = 1
      };
    let d = Domain.spawn (fun _ -> loop id) in
    (get_domain_info id).domain <- Some d;
    incr spawned_actors;
    id
  end else begin
    let info = get_domain_info (id mod max_domains) in
    info.running_actors_count <- info.running_actors_count + 1;
    id
  end
