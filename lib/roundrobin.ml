open Effect
open Effect.Deep

exception Stop
exception Interrupt

type process = unit -> unit
type t = {
  processes : process Domainslib.Chan.t;
  mutable domain : unit Domain.t Option.t
}


let push_process fifo process =
  Domainslib.Chan.send fifo.processes process


let get_process fifo =
  Domainslib.Chan.recv fifo.processes


let manage_next_process fifo =
  get_process fifo ()


type _ Effect.t += WaitFor : (unit -> bool) -> unit Effect.t
let wait_for condition =
  perform @@ WaitFor condition

type _ Effect.t += Yield : unit Effect.t
let yield () =
  perform @@ Yield

let rec loop fifo =
  match_with manage_next_process fifo {
    retc = (fun _ -> loop fifo);
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
                push_process fifo (fun _ -> continue k v));
            loop fifo;
        )
      | Promise.Get p -> Some (
          fun (k : (a, _) continuation) ->
            while Fun.negate Promise.is_ready p do
              Domain.cpu_relax ()
            done;
            continue k (Promise.get p);
            loop fifo;
        )
      | Promise.Async f -> Some (
          fun (k : (a, _) continuation) ->
            push_process fifo f;
            continue k ()
        )
      | WaitFor condition -> Some (
          fun (k : (a, _) continuation) ->
            if condition () then
              continue k ()
            else (
              push_process fifo (fun _ ->
                  wait_for condition; continue k ());
              loop fifo
            )
        )
      | _ -> None
  }

let stop fifo =
  push_process fifo (fun _ -> Gc.full_major (); raise Stop);
  try Domain.join (Option.get fifo.domain) with
  | Stop -> ();
    fifo.domain <- None

let create () =
  let a = {
    processes = Domainslib.Chan.make_unbounded ();
    domain = None
  } in
  let d = Domain.spawn (fun _ -> loop a) in
  a.domain <- Some d;
  a, Domain.get_id d
