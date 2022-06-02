(* Module Actor *)
open Effect.Deep

type 'a process = (unit -> unit)

(* Type of actor, 's is the type of messages,
   'a is the return type of methods *)
(* NOTE: It would be great if 'a was part of 's *)
(* It is possible with a GADT like so: *)
(* type 'a message =
     | Fib : int -> int message *)
(*   | Flip : ('a -> 'b -> 'c) * 'a * 'b -> 'c message *)

(* But the actor would be parameterized on 's, of kind (* -> *), *)
(* I Don't know if it's possible in OCaml *)
type ('m, 's, 'a) t = {
  processes : 'a process Queue.t;
  memory : 'm Domain.DLS.key;
  methods : ('m, 's, 'a) t -> 's -> 'a
}

let create init methods = {
  processes = Queue.create ();
  memory = Domain.DLS.new_key init;
  methods = methods
}

let get_memory self = Domain.DLS.get self.memory
let set_memory self memory = Domain.DLS.set self.memory memory

let push_process self process =
  Queue.push process self.processes

let get_process self =
  Queue.take_opt self.processes

let send self message =
  let p = Promise.create () in
  push_process self (fun () ->
      Promise.fill p (self.methods self message));
  p

let manage_next_process self =
  match get_process self with
  | None -> ()
  | Some f -> f ()

let run self = ignore @@ Domain.spawn (fun _ ->
    let rec loop () =
      match_with manage_next_process self {
        retc = (fun _ -> loop ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Promise.NotReady p -> Some (
              fun (k : (a, _) continuation) ->
                (* The process is waiting for the promise to be filled *)
                (* So we add a hook to this promise to push the process *)
                (* back to the queue *)
                Promise.add_hook p (fun v ->
                    push_process self (fun _ -> continue k v));
                loop ();
            )
          | _ -> None
      }
    in loop ()
  )
