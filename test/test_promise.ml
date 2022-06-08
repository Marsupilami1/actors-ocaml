(* Test the functions of the Promise Module *)

open Actorsocaml
open Promise.Infix

let test1 () =
  let p = Promise.pure 8 in
  let k = Promise.fmap ((+) 34) p in
  assert (42 = Promise.get k);
  print_endline "Test 1 : Passed"

let test2 () =
  let p = Promise.pure 17 in
  let k = p >>= (fun v -> Promise.pure (v + 25)) in
  assert (42 = Promise.get k);
  print_endline "Test 2 : Passed"

let test3 () =
  let p = Promise.pure 21 in
  let pf = Promise.pure (( * ) 2) in
  let k = pf <*> p in
  assert (42 = Promise.get k);
  print_endline "Test 3 : Passed"

let test4 () =
  let (p, fill) = Promise.create () in
  Promise.add_callback p (fun v -> assert (v == 42));
  fill 42;
  print_endline "Test 4 : Passed"

let test5 () =
  let (p, fillp) = Promise.create () in
  let (q, fillq) = Promise.create () in
  let k = Promise.join p in
  fillp q;
  fillq 42;
  assert (42 = Promise.get k);
  print_endline "Test 5 : Passed"

let test6 () =
  let (p, fillp) = Promise.create () in
  let (q, _) = Promise.create () in
  Promise.add_callback q (fun v -> assert (v = 42));
  Promise.unify q p;
  fillp 42;
  assert (Promise.get p = 42);
  assert (Promise.get q = 42);
  print_endline "Test 6 : Passed"


let _ =
  print_endline "-----TEST PROMISE-----";
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  print_endline "----------------------"
