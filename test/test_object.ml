open Actorsocaml

(* problem with `val`: *)
(* they are refered as `value` in the code, so it's difficult *)
(* to convert at ppx time (I think). *)
(* We need to wrap them into DLS: *)
(* - 1 per variable seems overkill; actually no *)
(* - 1 for all is fine, but implies to construct a record at compile time *)

(* same problem for `self` *)

let x =
  object%actor
    val mutable y = 0
    method set n = y <- n
    (* () are mandatory (otherwise we get the same value again and again) *)
    method get () = y
  end

let ping =
  object%actor (self)
    method ping pong n =
      Printf.printf "Ping: %d\n%!" n;
      if n <= 0 then ()
      else Promise.await @@ pong#!pong self (n - 1)
  end
let pong =
  object%actor (self)
    method pong ping n =
      Printf.printf "Pong: %d\n%!" n;
      if n <= 0 then ()
      else Promise.await @@ ping#!ping self (n - 1)
  end

let main _ =
  Promise.get @@ x#!set 42;
  Printf.printf "%d\n" @@ Promise.await @@ x#!get ();
  Promise.await @@ ping#!ping pong 10


let _ = Actor.Main.run main
