open Actorsocaml
open Promise.Infix

let mk_fib () = object%actor (self)
  val a = Array.make 1000 None
  method compute n =
    if a.(n) <> None then
      Option.get a.(n)
    else begin
      let result : int Promise.t =
        if n < 2 then
          Promise.pure n
        else
          let* f1 = Promise.join @@ self#!compute (n - 1)
          and* f2 = Promise.join @@ self#!compute (n - 2)
          in Promise.pure (f1 + f2)
      in
      a.(n) <- Option.some result;
      result
    end
end

let test _ =
  print_endline "-----TEST ACTOR-----";

  let n = 42 in
  let fib = mk_fib () in
  let p = Promise.join @@ fib#!compute n in
  Alcotest.(check int) "same int" 267914296 (Promise.await p);

  print_endline "Test passed";
  print_endline "--------------------"


let main _ =
  let open Alcotest in
  run "Actorsocaml Actor test" [
    "Actor", [
      test_case "fib" `Quick test;
    ];
  ]

let _ = Actor.Main.run main
