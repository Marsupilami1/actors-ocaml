(* Module Promise *)
(* A promise is a "write-once" box *)

open Effect

(* Status of a promise *)
type 'a status =
  (* An empty promise accumulates callbacks to be executed when it is filled *)
  | Empty of ('a -> unit) list
  | Filled of 'a
  | Forwarded of 'a t Atomic.t
and 'a t = 'a status Atomic.t

let create () = Atomic.make @@ Empty []

type _ Effect.t += NotReady : 'a t -> 'a Effect.t

let rec find_leader p =
  let x = Atomic.get p in
  match Atomic.get x with
  | Forwarded p' ->
    let leader = find_leader p' in
    leader
    (* if Atomic.compare_and_set p x (leader) *)
    (* then leader *)
    (* else find_leader p *)
  | _ -> x

let rec await p =
  match Atomic.get p with
  | Empty _ -> perform @@ NotReady p
  | Filled v -> v
  | Forwarded p' -> await (find_leader p')

(* get is blocking *)
let rec get p =
  match Atomic.get p with
  | Empty _ -> Domain.cpu_relax (); get p
  | Filled v -> v
  | Forwarded p' -> get (find_leader p')

exception Promise__Multiple_Write

let apply_callbacks l v =
  List.iter (fun f -> f v) l

let rec fill p v =
  match Atomic.get p with
  | Empty l as x ->
    if Atomic.compare_and_set p x (Filled v) then
      apply_callbacks l v
    else fill p v;
  | Filled _ -> raise Promise__Multiple_Write
  | Forwarded p' -> fill (find_leader p') v

let rec is_ready p =
  match Atomic.get p with
  | Empty _ -> false
  | Filled _ -> true
  | Forwarded p' -> is_ready (find_leader p')

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
  | Forwarded p' -> add_callback (find_leader p') f

let rec unify_leaders p p' =
  if p == p' then print_endline "beng" else begin
    match Atomic.get p, Atomic.get p' with
    | Filled v, (Empty l as x) ->
      if Atomic.compare_and_set p' x (Forwarded(Atomic.make p)) then
        apply_callbacks l v
      else unify_leaders p p'
    | Filled _, Filled _ -> raise Promise__Multiple_Write
    | Filled _, Forwarded q -> unify_leaders p (Atomic.get q)
    | _, Filled _ -> unify_leaders p' p
    | (Empty l as x), Empty _ ->
      if Atomic.compare_and_set p x (Forwarded (Atomic.make p')) then
        add_callback p' (apply_callbacks l)
      else unify_leaders p p'
    | Forwarded q, Forwarded q' -> unify_leaders (Atomic.get q) (Atomic.get q')
    | Empty _, Forwarded q -> unify_leaders p (Atomic.get q)
    | Forwarded _, Empty _ -> unify_leaders p' p
  end

let unify p p' =
  unify_leaders p p'

let fmap f p =
  let p' = create () in
  add_callback p (fun v -> fill p' (f v));
  p'

let pure v = Atomic.make (Filled v)

let join pp =
  let res = create () in
  add_callback pp (fun p -> add_callback p (fun v -> fill res v));
  res

let bind m f =
  let p = create () in
  add_callback m (fun v -> fill p (f v));
  join p


module Infix = struct
  let (<$>) = fmap
  let (<*>) pf px =
    let p = create () in
    add_callback pf (
      fun f -> add_callback px (
          fun x -> fill p (f x)
        )
    );
    p
  let (>>=) = bind
  let (=<<) f m = m >>= f
end
