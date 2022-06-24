open Actorsocaml

let a = object%actor
  val y = 42
  method get = y
end

let b = object%actor
  val z = 67
  method get = a#!!get
end

let main () =
  let p = b#!get in
  Printf.printf "a.y = %d\n"  @@ Promise.get p

let _ = Actor.Main.run main
