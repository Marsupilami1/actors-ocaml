open Actorsocaml

let a = object%actor
  val y = 42
  method get = y
end

let b = object%actor
  val z = 67
  method foo = z
  method get = a#!!get
end

(* let foo : <foo : int; ..> = object (this) *)
(*   method private field = 0 *)
(*   method foo = *)
(*     let self = this in *)
(*     self#field *)
(* end *)


let main () =
  let p = b#!get in
  Printf.printf "a.y = %d\n"  @@ Promise.get p
(* Printf.printf "foo = %d\n"  @@ foo#field *)

let _ = Actor.Main.run main
