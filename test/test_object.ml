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
    val y = 42
    val _z = 42
    method get = y
  end

(* let y = *)
(*   object *)
(*     val _mem = Domain.DLS.new_key (fun _ -> (42,42)) *)
(*     method get = *)
(*       let (y, _z) = Domain.DLS.get _mem in *)
(*       y *)
(*   end *)

(* Argument self should be next to the `object` key word, *)
(* but I don't know how to solve the type mismatched. *)
(* self is a pattern, any or var *)
(* let ping = *)
(*   object%actor *)
(*     method ping self pong n = *)
(*       Printf.printf "Ping: %d\n%!" n; *)
(*       if n <= 0 then *)
(*         () *)
(*       else *)
(*         Promise.await @@ pong#!pong pong self (n - 1) *)
(*   end *)
(* let pong = *)
(*   object%actor *)
(*     method pong self ping n = *)
(*       Printf.printf "Pong: %d\n%!" n; *)
(*       if n <= 0 then *)
(*         () *)
(*       else *)
(*         Promise.await @@ ping#!ping ping self (n - 1) *)
(*   end *)

let main _ =
  Printf.printf "%d\n" @@ Promise.await x#!get
(* Promise.await @@ ping#!ping ping pong 10 *)


let _ = Actor.Main.run main
