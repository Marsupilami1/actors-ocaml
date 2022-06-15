open Actorsocaml

let main _ =
  let open Alcotest in
  run "Actorsocaml tests" [
    "Promise", [
      test_case "fmap"        `Quick Test_promise.test_fmap;
      test_case "bind"        `Quick Test_promise.test_bind;
      test_case "applicative" `Quick Test_promise.test_applicative;
      test_case "callback"    `Quick Test_promise.test_callback;
      test_case "join"        `Quick Test_promise.test_join;
      test_case "unify"       `Quick Test_promise.test_unify;
      test_case "ring unify"  `Quick Test_promise.test_ring_unify;
      test_case "let"         `Quick Test_promise.test_let;
      test_case "async"       `Quick Test_promise.test_async;
    ];
    "Actor", [
      test_case "fib" `Quick Test_actor.test;
      test_case "wait_for" `Quick Test_condition.test;
    ];
  ]

let _ = Actor.Main.run main
