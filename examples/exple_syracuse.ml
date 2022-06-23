open Actorsocaml

let actor = object%actor (self)
  method syracuse n =
    if n = 1 then 1
    else begin
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      Promise.await @@ self#!syracuse next
    end
end

let main _ =
  let n = 989345275647 in
  let p = Promise.await @@ actor#!syracuse n in
  assert (1 = p)

let _ = Actor.Main.run main
