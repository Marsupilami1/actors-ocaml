(* Module Promise *)
(* A promise is a "write-once" box *)

open Effect

(* Status of a promise *)
type 'a status =
  (* An empty promise accumulates callbacks to be executed when it is filled *)
  | Empty of ('a -> unit) list
  | Filled of 'a

type 'a t = 'a status ref

let create () = ref @@ Empty []

type _ Effect.t += NotReady : 'a t -> 'a Effect.t

let await p =
  match !p with
  | Empty _ -> perform @@ NotReady p
  | Filled v -> v

(* get is blocking *)
let rec get p =
  match !p with
  | Empty _ -> Domain.cpu_relax (); get p
  | Filled v -> v

exception Future__Multiple_Write

let fill p v =
  match !p with
  | Empty l ->
    (* run all callbacks *)
    List.iter (fun f -> f v) l;
    p := Filled v;
  | Filled _ -> raise Future__Multiple_Write

let is_ready p =
  match !p with
  | Empty _ -> false
  | Filled _ -> true

(* Add a callback to the given promise *)
(* If it's already filled, just run the callback *)
(* You should not create a computation-intensive callback, because *)
(* it will be executed by the promise filler, which can run in *)
(* another thread. *)
let add_callback p f =
  match !p with
  | Empty l -> p := Empty(f :: l)
  | Filled v -> f v

let fmap f p =
  let p' = create () in
  add_callback p (fun v -> fill p' (f v));
  p'

let pure v = ref (Filled v)

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
