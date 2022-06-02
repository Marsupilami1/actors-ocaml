(* Module Actor *)
open Effect.Deep

type 'a process =
  | MailReader
  | Fill of 'a Promise.t * (unit -> 'a)
  | Computation of (unit -> unit)

(* For debugging purpose *)
(* let print_process = function *)
(*   | MailReader -> print_endline "MailReader" *)
(*   | Fill _ -> print_endline "Fill" *)
(*   | Computation _ -> print_endline "Computation" *)

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
  mail_box : ('s * 'a Promise.t) Queue.t;
  mail_mutex : Mutex.t;
  processes : 'a process Queue.t;
  (* TODO: the memory should be attached to the Domain *)
  memory : 'm;
  methods : ('m, 's, 'a) t -> 's -> 'a
}

let create memory methods = {
  mail_box = Queue.create ();
  mail_mutex = Mutex.create ();
  processes = Queue.create ();
  memory = memory;
  methods = methods
}

let send self message =
  Mutex.lock self.mail_mutex;
  let p = Promise.create () in
  Queue.push (message, p) self.mail_box;
  Mutex.unlock self.mail_mutex;
  p

let memory self = self.memory

let push_process self process =
  Queue.push process self.processes

let get_process self =
  Queue.take_opt self.processes

let read_mails self =
  (* Do not spam the mail box *)
  Mutex.lock self.mail_mutex;
  if Queue.is_empty self.processes then Domain.cpu_relax ();
  Queue.iter (fun (m, p) ->
      push_process self @@ Fill(p, fun () -> self.methods self m)
    ) self.mail_box;
  (* Mutex on mailbox ? *)
  Queue.clear self.mail_box;
  Mutex.unlock self.mail_mutex

let manage_next_process self =
  (* Never empty because of the mail_reader *)
  let process = Option.get @@ get_process self in
  begin
    match process with
    | MailReader ->
      read_mails self;
      push_process self MailReader
    | Fill (p, f) ->
      let v = f () in Promise.fill p v
    | Computation f -> f ()
  end

let run self = ignore @@ Domain.spawn (fun _ ->
    push_process self MailReader;
    let rec loop () =
      match_with manage_next_process self {
        retc = (fun _ -> loop ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Promise.NotReady p -> Some (
              fun (k : (a, _) continuation) ->
                (* The process is waiting for the promise to be filled *)
                (* So we add a hook to this promise to pusn the process *)
                (* back to the queue *)
                Promise.add_hook p (fun v ->
                    push_process self
                      (Computation (fun _ -> continue k v))
                  );
                loop ();
            )
          | _ -> None
      }
    in loop ()
  )
