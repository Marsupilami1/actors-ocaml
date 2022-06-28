open Actorsocaml

(* let foo : <foo : int; ..> = object (this) *)
(*   method private field = 0 *)
(*   method foo = *)
(*     let self = this in *)
(*     self#field *)
(* end *)


let main () =
let a = object%actor
  val mutable y = 0
  method get = y
  method set n = y <- n
end in

let b = object%actor
  method get = a#!!get
end in
  a#.set 42;
  let p = b#!get in
  Printf.printf "a.y = %d\n"  @@ Promise.get p
(* Printf.printf "foo = %d\n"  @@ foo#field *)

let _ = Actor.Main.run main
