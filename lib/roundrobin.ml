open Effect
open Effect.Deep

exception Stop
exception Interrupt

type process = unit -> unit
type t = { processes : process Domainslib.Chan.t }

let create () = {
  processes = Domainslib.Chan.make_unbounded ()
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


let rec run fifo =
  match_with manage_next_process fifo {
    retc = (fun _ -> run fifo);
    exnc = (fun e -> match e with
        | Interrupt -> run fifo
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
            run fifo;
        )
      | WaitFor condition -> Some (
          fun (k : (a, _) continuation) ->
            if condition () then
              continue k ()
            else (
              push_process fifo (fun _ ->
                  wait_for condition; continue k ());
              run fifo
            )
        )
      | _ -> None
  }


let stop fifo =
  push_process fifo (fun _ -> raise Stop);
