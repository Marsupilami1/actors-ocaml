open Actorsocaml

module MyMessage = struct
  type 'a t =
    | Set : int -> unit t
    | Wait : unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

type memory = int
let init () = 0

module MyActor = Actor.Make(MyMessage)

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


let _ =
  print_endline "-----TEST Condition-----";
  let actor = MyActor.create init actor_methods in
  let ra = MyActor.run actor in

  let pw = MyActor.send actor Wait in
  let _ = MyActor.send actor (Set 41) in
  let _ = MyActor.send actor (Set 42) in
  Promise.get pw;

  MyActor.stop actor ra;
  print_endline "Test passed";
  print_endline "--------------------"
