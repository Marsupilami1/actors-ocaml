(* Test the functions of the Promise Module *)
open Actorsocaml
open Promise.Infix

let test_fmap () =
  let p = Promise.pure 8 in
  let k = Promise.fmap ((+) 34) p in
  Alcotest.(check int) "same int" 42 (Promise.await k);
  print_endline "Test 1 : Passed"

let test_bind () =
  let p = Promise.pure 17 in
  let k = p >>= (fun v -> Promise.pure (v + 25)) in
  Alcotest.(check int) "same int" 42 (Promise.await k);
  print_endline "Test 2 : Passed"

let test_applicative () =
  let p = Promise.pure 21 in
  let pf = Promise.pure (( * ) 2) in
  let k = pf <*> p in
  Alcotest.(check int) "same int" 42 (Promise.await k);
  print_endline "Test 3 : Passed"

let test_callback () =
  let (p, fill) = Promise.create () in
  Promise.add_callback p (fun v ->
      Alcotest.(check int) "same int" 42 v);
  fill 42;
  print_endline "Test 4 : Passed"

let test_join () =
  let (p, fillp) = Promise.create () in
  let (q, fillq) = Promise.create () in
  let k = Promise.join p in
  fillp q;
  fillq 42;
  Alcotest.(check int) "same int" 42 (Promise.await k);
  print_endline "Test 5 : Passed"

let test_unify () =
  let (p, fillp) = Promise.create () in
  let (q, _) = Promise.create () in
  Promise.add_callback q (fun v ->
      Alcotest.(check int) "same int" 42 v);
  Promise.unify q p;
  fillp 42;
  Alcotest.(check int) "same int" 42 (Promise.await p);
  Alcotest.(check int) "same int" 42 (Promise.await q);
  print_endline "Test 6 : Passed"

let test_ring_unify () =
  let p, fillp = Promise.create () in
  let q, _ = Promise.create () in
  Promise.unify p q;
  Promise.unify q p;
  fillp 42;
  Alcotest.(check int) "same int" 42 @@ Promise.await q;
  Alcotest.(check int) "same int" 42 @@ Promise.await p

(*  for n = 8: *)
(* O <- filled *)
(* +----O      *)
(* O    +-O    *)
(* +-O  O |    *)
(* O |  | O    *)
(* | O  O-+    *)
(* O-+    O    *)
(*   O----+    *)
(*        O    *)
let test_huge_unify () =
  let n = 128 in
  let t = Array.init n (fun _ -> Promise.create ()) in
  let t' = Array.init n (fun _ -> Promise.create ()) in
  for i=1 to (n/2 - 1) do
    Promise.unify (fst t.(i)) (fst (t.(2 * i)));
    Promise.unify (fst t.(i)) (fst (t.(2 * i + 1)));
    Promise.unify (fst t'.(i)) (fst (t'.(2 * i)));
    Promise.unify (fst t'.(i)) (fst (t'.(2 * i + 1)));
  done;
  for i=(n/2) to (n-1) do
    Promise.unify (fst t.(i)) (fst (t'.(i)));
  done;
  (snd t.(1)) 42;
  for i=1 to (n-1) do
    Alcotest.(check int) "same int" 42 @@ Promise.await (fst t.(i));
    Alcotest.(check int) "same int" 42 @@ Promise.await (fst t'.(i));
  done

let test_let () =
  let p = begin
    let* x = Promise.pure 40
    and* y = Promise.pure 2 in
    Promise.pure @@ x + y
  end in
  Alcotest.(check int) "same int" 42 (Promise.await p);
  print_endline "Test 7 : Passed"

let test_async () =
  let p = Promise.async (fun () -> 42) in
  Alcotest.(check int) "same int" 42 (Promise.await p);
  print_endline "Test 8 : Passed"

let test _ =
  let open Alcotest in
  run "Actorsocaml tests" [
    "Promise", [
      test_case "fmap"        `Quick test_fmap;
      test_case "bind"        `Quick test_bind;
      test_case "applicative" `Quick test_applicative;
      test_case "callback"    `Quick test_callback;
      test_case "join"        `Quick test_join;
      test_case "unify"       `Quick test_unify;
      test_case "ring unify"  `Quick test_ring_unify;
      test_case "huge unify"  `Quick test_huge_unify;
      test_case "let"         `Quick test_let;
      test_case "async"       `Quick test_async;
    ];
  ]

let _ = Actor.Main.run test
