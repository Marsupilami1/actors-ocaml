open Actorsocaml

(* Simple message for simple actor *)
type signal = Fib of int | ToZero of int

let rec actor : (signal, int) Actor.t = {
  fifo = Queue.create ();
  methods = function
    |Fib n ->
      if n < 2 then
        n
      else begin
        let a = Actor.send actor (Fib (n - 1)) in
        let b = Actor.send actor (Fib (n - 2)) in
        (* need to be done after the request *)
        Promise.get_val a + Promise.get_val b
      end
    | ToZero n ->
      if n = 0 then
        n
      else
        let a = Actor.send actor (ToZero (n - 1)) in
        Promise.get_val a
};;

let _main =
  print_endline "Test";
  let run () =
    let f = Actor.send actor (Fib 8) in (* client code *)
    (* while not @@ Queue.is_empty actor.fifo do (\* actor loop *\) *)
      (* let (message, f) = Queue.take actor.fifo in *)
      (* Promise.fill (fun () -> actor.methods message) f; *)
    (* done; *)
    let t = Promise.get_val f (* client code *) in
    Printf.printf "%d\n" t
  in
  match Promise.run run with
  | Ok(_) -> ()
  | Error(e) -> raise e
