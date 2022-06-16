open Actorsocaml

(* problem with `val`: *)
(* they are refered as `value` in the code, so it's difficult *)
(* to convert at ppx time (I think). *)
(* We need to wrap them into DLS: *)
(* - 1 per variable seems overkill *)
(* - 1 for all is fine, but implies to construct a record at compile time *)

(* same problem for `self` *)

let x : <get : int> Oactor.t =
  object%actor (_self)
    val y = let m = Domain.DLS.new_key (fun _ -> 42) in m
    method get = Domain.DLS.get y
  end

(* Argument self should be next to the `object` key word, *)
(* but I don't know how to solve the type mismatched. *)
let ping =
  object%actor
    method ping self pong n =
      Printf.printf "Ping: %d\n%!" n;
      if n <= 0 then
        ()
      else
        Promise.await @@ pong#!pong pong self (n - 1)
  end
let pong =
  object%actor
    method pong self ping n =
      Printf.printf "Pong: %d\n%!" n;
      if n <= 0 then
        ()
      else
        Promise.await @@ ping#!ping ping self (n - 1)
  end

let main _ =
  Printf.printf "%d\n" @@ Promise.await @@ x#!get;
  Promise.await @@ ping#!ping ping pong 10


let _ = Actor.Main.run main
