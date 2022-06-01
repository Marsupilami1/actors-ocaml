(* Module Promise *)
(* A promise is a "write-once" box *)

open Effect

(* Status of a promise *)
type 'a status = Empty | Filled of 'a

type 'a t = 'a status ref

let create () = ref Empty

type _ Effect.t += NotReady : 'a t -> 'a Effect.t

(* get is blocking *)
let get p =
  match !p with
  | Empty -> perform @@ NotReady p
  | Filled v -> v

let rec wait_and_get p =
  match !p with
  | Empty -> Domain.cpu_relax (); wait_and_get p
  | Filled v -> v

exception Future__Multiple_Write

let fill p v =
  match !p with
  | Empty -> p := Filled v
  | Filled _ -> raise Future__Multiple_Write

let is_ready p = !p <> Empty
