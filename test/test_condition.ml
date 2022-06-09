open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Set : int -> unit t
    | Wait : unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

type memory = int
let init () = 0

module MyActor = Actor.Make(Roundrobin)(MyMessage)

let methods
  : type a . memory MyActor.t
    -> (a Promise.t -> a)
    -> a MyMessage.t
    -> a
  = fun self _ -> function
  | Set n ->
    Printf.printf "Set n to %d\n%!" n;
    MyActor.set_memory self n
  | Wait ->
    print_endline "Wait for n = 42";
    MyActor.wait_for (fun _ ->
        MyActor.get_memory self = 42
      );
    print_endline "n is now 42"
let actor_methods self = {
  MyMessage.m = fun s -> methods self s
}


let main _ =
  print_endline "-----TEST Condition-----";
  let actor = MyActor.create init actor_methods in
  MyActor.run actor;

  let pw = MyActor.send actor Wait in
  let _ = MyActor.send actor (Set 41) in
  let _ = MyActor.send actor (Set 42) in
  Promise.await pw;

  MyActor.stop actor;
  print_endline "Test passed";
  print_endline "--------------------"

let _ = Actor.Main.run main
