(* Module Promise *)
(* A promise is a "write-once" box *)

open Effect

(* Status of a promise *)
type 'a status =
  (* An empty promise accumulates callbacks to be executed when it is filled *)
  | Empty of ('a -> unit) list
  | Filled of 'a
  | Failed of exn
and 'a t = 'a status Atomic.t

type 'a resolver = 'a t

exception Promise__Multiple_Write

let apply_callbacks l v =
  List.iter (fun f -> f v) l

let rec resolve p v =
  match Atomic.get p with
  | Empty l as x  ->
    if Atomic.compare_and_set p x (Filled v) then
      apply_callbacks l v
    else resolve p v;
  | _ -> raise Promise__Multiple_Write

let rec fail p e =
  match Atomic.get p with
  | Empty _ as x  ->
    if Atomic.compare_and_set p x (Failed e) then
      ()
    else fail p e;
  | _ -> raise Promise__Multiple_Write


let create () =
  let p = Atomic.make @@ Empty [] in
  (p, p)

type _ Effect.t += NotReady : 'a t -> 'a Effect.t
type _ Effect.t += Get : 'a t -> 'a Effect.t


let await p =
  match Atomic.get p with
  | Empty _ -> perform @@ NotReady p
  | Filled v -> v
  | Failed e -> raise e

let await_or_exn p =
  match Atomic.get p with
  | Empty _ -> let v = perform @@ NotReady p in Ok v
  | Filled v -> Ok v
  | Failed e -> Error e

let get p = match Atomic.get p with
  | Empty _ -> perform @@ Get p
  | Filled v -> v
  | Failed e -> raise e

let get_or_exn p = match Atomic.get p with
  | Empty _ -> let v = perform @@ Get p in Ok v
  | Filled v -> Ok v
  | Failed e -> Error e

let is_ready p =
  match Atomic.get p with
  | Empty _ -> false
  | _ -> true

(* Add a callback to the given promise *)
(* If it's already filled, just run the callback *)
(* You should not create a computation-intensive callback, because *)
(* it will be executed by the promise filler, which can run in *)
(* another thread. *)
let rec add_callback p f =
  match Atomic.get p with
  | Empty l as x ->
    if Atomic.compare_and_set p x (Empty (f::l)) then
      ()
    else add_callback p f
  | Filled v -> f v
  | Failed e -> raise e

let fmap f p =
  let (p', r') = create () in
  add_callback p (fun v -> resolve r' (f v));
  p'

let pure v = Atomic.make (Filled v)

let return = pure

let never_resolve () = Atomic.make @@ Failed Promise__Multiple_Write

let join pp =
  let (res, r) = create () in
  add_callback pp (fun p -> add_callback p (fun v -> resolve r v));
  res

let bind m f =
  let (p, r) = create () in
  add_callback m (fun v -> resolve r (f v));
  join p


type _ Effect.t += Async : 'a resolver * (unit -> 'a) -> unit Effect.t
let async f =
  let (p, r) = create () in
  perform (Async (r, f));
  p

module Infix = struct
  let (<$>) = fmap
  let (<*>) pf px =
    let (p, r) = create () in
    add_callback pf (
      fun f -> add_callback px (
          fun x -> resolve r (f x)
        )
    );
    p
  let (>>=) = bind
  let (=<<) f m = m >>= f
  let (>>) m m' = m >>= (fun _ -> m')

  let ( let+ ) p f = f <$> p
  let ( and+ ) x y = (fun x y -> (x, y)) <$> x <*> y

  let ( let* ) = (>>=)
  let ( and* ) = ( and+ )
end
