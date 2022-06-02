(* Test the functions of the Promise Module *)

open Actorsocaml
open Promise.Infix

let test1 () =
  let p = Promise.pure 8 in
  let k = Promise.fmap ((+) 34) p in
  assert (42 = Promise.wait_and_get k);
  print_endline "Test 1 : Passed"

let test2 () =
  let p = Promise.pure 17 in
  let k = p >>= (fun v -> Promise.pure (v + 25)) in
  assert (42 = Promise.wait_and_get k);
  print_endline "Test 2 : Passed"

let test3 () =
  let p = Promise.pure 21 in
  let pf = Promise.pure (( * ) 2) in
  let k = pf <*> p in
  assert (42 = Promise.wait_and_get k);
  print_endline "Test 3 : Passed"

let test4 () =
  let p = Promise.create () in
  Promise.add_hook p (fun v -> assert (v == 42));
  Promise.fill p 42;
  print_endline "Test 4 : Passed"

let test5 () =
  let p = Promise.create () in
  let q = Promise.create () in
  let k = Promise.join p in
  Promise.fill p q;
  Promise.fill q 42;
  assert (42 = Promise.wait_and_get k);
  print_endline "Test 5 : Passed"



let _ =
  print_endline "-----TEST PROMISE-----";
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  print_endline "----------------------"
