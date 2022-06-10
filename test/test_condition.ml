open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Set : int -> unit t
    | Wait : unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(Roundrobin)(MyMessage)

let actor_methods =
  let init () = 0 in
  let mem = Domain.DLS.new_key init in
  let methods
    : type a . MyActor.t
      -> (a Promise.t -> a)
      -> a MyMessage.t
      -> a
    = fun _ _ -> function
      | Set n ->
        Printf.printf "Set n to %d\n%!" n;
        Domain.DLS.set mem n
      | Wait ->
        print_endline "Wait for n = 42";
        MyActor.wait_for (fun _ ->
            42 = Domain.DLS.get mem
          );
        print_endline "n is now 42"
  in
  fun self -> {MyMessage.m = fun f -> methods self f}


let main _ =
  print_endline "-----TEST Condition-----";
  let actor = MyActor.create actor_methods in
  MyActor.run actor;

  let pw = MyActor.send actor Wait in
  let _ = MyActor.send actor (Set 41) in
  let _ = MyActor.send actor (Set 42) in
  Promise.await pw;

  print_endline "Test passed";
  print_endline "--------------------"

let _ = Actor.Main.run main
