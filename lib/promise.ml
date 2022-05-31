include Effect
include Effect.Deep

type cont =
  | Cont : (unit, 'b) continuation -> cont

type tvar = cont option ref

let mk_tvar k = ref (Some (Cont k))

type 'a status =
  | Invalid
  | Done of 'a
  | Cancelled of exn
  | Waiting of tvar list

let print_status s =
  match !s with
  | Invalid -> print_endline "Invalid"
  | Done _ -> print_endline "Done"
  | Cancelled _ -> print_endline "Cancelled"
  | Waiting _ -> print_endline "Waiting"

type 'a t = 'a status ref

type _ Effect.t += Fill : (unit -> 'a) * 'a t -> unit Effect.t
type _ Effect.t += Wait : 'a t -> unit Effect.t

let make_empty () = ref Invalid

let fill f p =
  perform (Fill (f, p))


let enqueue run_q k v =
  Queue.push (fun () -> ignore @@ continue k v) run_q

let dequeue run_q =
  if Queue.is_empty run_q then ()
  else (Queue.pop run_q) ()

let finish run_q sr v =
  match !sr with
  | Waiting l ->
    sr := Done v;
    List.iter (fun tv ->
        match !tv with
        | None -> ()
        | Some (Cont k) ->
          tv := None;
          enqueue run_q k ()) l
  | _ -> failwith "Impossible: finish"

let abort run_q sr e =
  match !sr with
  | Waiting l ->
    sr := Cancelled e;
    List.iter (fun tv ->
        match !tv with
        | None -> ()
        | Some (Cont k) ->
          tv := None;
          enqueue run_q k ()) l
  | _ -> failwith "Impossible: abort"

let wait sr k =
  match !sr with
  | Waiting l -> sr := Waiting (mk_tvar k::l)
  | _ -> failwith "Impossible: wait"

let rec get sr =
  match !sr with
  | Invalid -> failwith "Cannot read empty promise"
  | Done v -> Ok v
  | Cancelled e -> Error e
  | Waiting _ -> perform (Wait sr); get sr

let rec get_val sr =
  match !sr with
  | Invalid -> failwith "Cannot read empty promise"
  | Done v -> v
  | Cancelled e -> raise e
  | Waiting _ -> perform (Wait sr); get_val sr

let pure v = ref (Done v)

let rec (<*>) f g =
  match !f, !g with
  | Invalid, _ -> failwith "Cannot read empty promise"
  | _, Invalid -> failwith "Cannot read empty promise"
  | Cancelled _ as x, _ -> ref x
  | _, (Cancelled _ as x) -> ref x
  | Waiting _, _ ->
    begin
      perform (Wait f);
      match get f with
      | Ok f -> ref (Done f) <*> g
      | Error e -> ref (Cancelled e)
    end
  | Done f, Done g -> ref (Done (f g))
  | Done f, Waiting _ ->
    begin
      perform (Wait g);
      match get g with
      | Ok g -> ref (Done (f g))
      | Error e -> ref (Cancelled e)
    end

let (>>=) m f =
  match !m with
  | Invalid -> failwith "Cannot read empty promise"
  | Done v -> f v
  | Cancelled _ as x -> ref x
  | Waiting _ -> begin
      perform (Wait m);
      match get m with
      | Ok v -> f v
      | Error e -> ref (Cancelled e)
    end

let forward f p =
  let p' = f () in
  let a = get_val p' in
  p := Done a

let run main =
  let run_q = Queue.create () in
  let rec spawn : 'a . 'a status ref -> (unit -> 'a) -> unit =
    fun sr f ->
      match_with f () {
        retc = (fun v -> finish run_q sr v; dequeue run_q);
        exnc = (fun e -> abort run_q sr e; dequeue run_q);
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Wait sr -> Some (
              fun (k : (a, _) continuation) ->
                wait sr k;
                dequeue run_q
            )
          | Fill(f, p) -> Some (
              fun (k : (a, _) continuation) ->
                p := Waiting [];
                enqueue run_q k ();
                spawn p f;
            )
          | _ -> print_endline "wtf"; None
      }
  in
  let sr = ref (Waiting []) in
  spawn sr main;
  get sr (* effects not handled here *)
