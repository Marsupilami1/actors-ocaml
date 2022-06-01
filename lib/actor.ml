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
type ('s, 'a) t = {
  mail_box : ('s * 'a Promise.t) Queue.t;
  processes : 'a process Queue.t;
  methods : ('s, 'a) t -> 's -> 'a
}

let create methods = {
  mail_box = Queue.create ();
  processes = Queue.create ();
  methods = methods
}

let send self message =
  let p = Promise.create () in
  Queue.push (message, p) self.mail_box;
  p

let get_message self =
  Queue.take_opt self.mail_box

let push_process self process =
  Queue.push process self.processes

let get_process self =
  Queue.take_opt self.processes

(* TODO: read all the mails and not just one *)
let read_mails self =
  if Queue.is_empty self.processes then Domain.cpu_relax ();
  match get_message self with
  | None -> ()
  | Some (m, p) ->
    push_process self @@ Fill(p, fun () -> self.methods self m)

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
                push_process self (
                  Computation(fun _ ->
                      continue k (Promise.get p))
                );
                loop ();
            )
          | _ -> None
      }
    in loop ()
  )
