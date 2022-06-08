open Actorsocaml


module MyMessage = struct
  type 'a t = Pass : unit -> unit t
  type method_type = { m : 'a . ('a Promise.t -> 'a) -> 'a t -> 'a }
end

module MyActor = Actor.Make(MyMessage)


let methods
  : type a . 'm MyActor.t
    -> (a Promise.t -> a)
    -> a MyMessage.t
    -> a =
  fun _ _ -> function
    | Pass () -> ()
let actor_methods self = {
  MyMessage.m = fun s -> methods self s
}

let init = Fun.id

let actor = MyActor.create init actor_methods

let _ =
  print_endline "-----TEST BENCH-----";

  let message = MyMessage.Pass () in
  let ra = MyActor.run actor in
  ignore @@ MyActor.send actor message;
  Unix.sleep 1;
  MyActor.stop actor ra;
  print_endline "Test passed";
  print_endline "--------------------"
