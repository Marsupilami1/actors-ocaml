include Effect
include Effect.Deep

type 'a status =
  | Done of 'a
  | Cancelled of exn
  | Waiting

type 'a t = 'a status ref

(* I think the following definition does not allow us to immediatly return a promise: *)
(* type _ Effect.t += Fork : (unit -> 'a) -> 'a t Effect.t *)
(* So I'll use this one *)
type _ Effect.t += Fork : (unit -> 'a) * 'a t -> unit Effect.t
type _ Effect.t += Wait : unit Effect.t

let make_empty () = ref Waiting

let fork (f, sr) = perform (Fork(f, sr))

let enqueue run_q k v =
  Queue.push (fun () -> ignore @@ continue k v) run_q

let dequeue run_q =
  if Queue.is_empty run_q then ()
  else (Queue.pop run_q) ()

let finish sr v =
  match !sr with
  | Waiting -> sr := Done v
  | _ -> failwith "Impossible: finish"

let abort sr e =
  match !sr with
  | Waiting -> sr := Cancelled e
  | _ -> failwith "Impossible: abort"

let rec get sr =
  match !sr with
  | Done v -> Ok v
  | Cancelled e -> Error e
  | Waiting -> perform Wait; get sr

let rec get_val sr =
  match !sr with
  | Done v -> v
  | Cancelled e -> raise e
  | Waiting -> perform Wait; get_val sr

let pure v = ref (Done v)

let rec (<*>) f g =
  match !f, !g with
  | Cancelled _ as x, _ -> ref x
  | _, (Cancelled _ as x) -> ref x
  | Waiting, _ ->
      begin
        perform Wait;
        match get f with
        | Ok f -> ref (Done f) <*> g
        | Error e -> ref (Cancelled e)
      end
  | Done f, Done g -> ref (Done (f g))
  | Done f, Waiting ->
      begin
        perform Wait;
        match get g with
        | Ok g -> ref (Done (f g))
        | Error e -> ref (Cancelled e)
      end

let (>>=) m f =
  match !m with
  | Done v -> f v
  | Cancelled _ as x -> ref x
  | Waiting -> begin
      perform Wait;
      match get m with
      | Ok v -> f v
      | Error e -> ref (Cancelled e)
    end

let run main =
  let run_q = Queue.create () in
  let rec spawn : 'a. 'a status ref -> (unit -> 'a) -> unit =
    fun sr f ->
      match_with f () {
        retc = (fun v -> finish sr v; dequeue run_q);
        exnc = (fun e -> abort sr e; dequeue run_q);
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Wait -> Some (fun (k : (a, _) continuation) ->
            enqueue run_q k ();
            dequeue run_q
          )
          | Fork(f, sr) -> Some (fun (k : (a, _) continuation) ->
            enqueue run_q k ();
            spawn sr f
          )
          | _ -> None
      }
  in
  let sr = make_empty () in
  spawn sr main;
  get sr
