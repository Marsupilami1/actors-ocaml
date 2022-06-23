open Actorsocaml

let a = object%actor
  val y = 42
  method get = y
end

let main () =
  let p = a#!get in
  Printf.printf "a.y = %d\n"  @@ Promise.get p

let _ = Actor.Main.run main
