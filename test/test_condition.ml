open Actorsocaml

let test _ =
  let actor = object%actor
    val mutable x = 0
    method set n = x <- n
    method wait =
      Multiroundrobin.wait_for (fun _ -> x = 42);
      Alcotest.(check int) "same int" 42 x
  end in

  let p = actor#!wait in
  let _ = actor#!set 41 in
  let _ = actor#!set 42 in
  Promise.await p

let main _ =
  let open Alcotest in
  run "Actorsocaml Condition test" [
    "Condition", [
      test_case "condition" `Quick test;
    ];
  ]

let _ = Actor.Main.run main
